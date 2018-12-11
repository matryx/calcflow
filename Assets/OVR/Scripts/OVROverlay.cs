/************************************************************************************

Copyright   :   Copyright 2014 Oculus VR, LLC. All Rights reserved.

Licensed under the Oculus VR Rift SDK License Version 3.3 (the "License");
you may not use the Oculus VR Rift SDK except in compliance with the License,
which is provided at the time of installation or download, or which
otherwise accompanies this software in either electronic or hard copy form.

You may obtain a copy of the License at

http://www.oculus.com/licenses/LICENSE-3.3

Unless required by applicable law or agreed to in writing, the Oculus VR SDK
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

************************************************************************************/

using UnityEngine;
using System;
using System.Collections;
using System.Runtime.InteropServices;
using VR = UnityEngine.VR;

/// <summary>
/// Add OVROverlay script to an object with an optional mesh primitive
/// rendered as a TimeWarp overlay instead by drawing it into the eye buffer.
/// This will take full advantage of the display resolution and avoid double
/// resampling of the texture.
/// 
/// If the texture is dynamically generated, as for an interactive GUI or
/// animation, it must be explicitly triple buffered to avoid flickering
/// when it is referenced asynchronously by TimeWarp, check OVRRTOverlayConnector.cs for triple buffers design
/// 
/// We support 3 types of Overlay shapes right now
///		1. Quad : This is most common overlay type , you render a quad in Timewarp space.
///		2. Cylinder: [Mobile Only][Experimental], Display overlay as partial surface of a cylinder
///			* The cylinder's center will be your game object's center
///			* We encoded the cylinder's parameters in transform.scale, 
///				**[scale.z] is the radius of the cylinder
///				**[scale.y] is the height of the cylinder
///				**[scale.x] is the length of the arc of cylinder
///		* Limitations
///				**Only the half of the cylinder can be displayed, which means the arc angle has to be smaller than 180 degree,  [scale.x] / [scale.z] <= PI
///				**Your camera has to be inside of the inscribed sphere of the cylinder, the overlay will be faded out automatically when the camera is close to the inscribed sphere's surface.
///				**Translation only works correctly with vrDriver 1.04 or above
///		3. Cubemap: Display overlay as a cube map
///		4. OffcenterCubemap: [Mobile Only] Display overlay as a cube map with a texture coordinate offset
///			* The actually sampling will looks like [color = texture(cubeLayerSampler, normalize(direction) + offset)] instead of [color = texture( cubeLayerSampler, direction )]
///			* The extra center offset can be feed from transform.position
///			* Note: if transform.position's magnitude is greater than 1, which will cause some cube map pixel always invisible 
///					Which is usually not what people wanted, we don't kill the ability for developer to do so here, but will warn out.
/// </summary>

public class OVROverlay : MonoBehaviour
{
	public enum OverlayShape
	{
		Quad = OVRPlugin.OverlayShape.Quad,       // Display overlay as a quad
		Cylinder = OVRPlugin.OverlayShape.Cylinder,   // [Mobile Only][Experimental] Display overlay as a cylinder, Translation only works correctly with vrDriver 1.04 or above 

		Cubemap = OVRPlugin.OverlayShape.Cubemap,    // Display overlay as a cube map

		OffcenterCubemap = OVRPlugin.OverlayShape.OffcenterCubemap,    // Display overlay as a cube map with a center offset 

	}

	public enum OverlayType
	{
		None,           // Disabled the overlay
		Underlay,       // Eye buffers blend on top
		Overlay,        // Blends on top of the eye buffer
		OverlayShowLod  // (Deprecated) Blends on top and colorizes texture level of detail
	};

#if UNITY_ANDROID && !UNITY_EDITOR
	const int maxInstances = 3;
#else
	const int maxInstances = 15;
#endif

	internal static OVROverlay[] instances = new OVROverlay[maxInstances];

	/// <summary>
	/// Specify overlay's type
	/// </summary>
	public OverlayType currentOverlayType = OverlayType.Overlay;

	/// <summary>
	/// If true, the texture's content is copied to the compositor each frame.
	/// </summary>
	public bool isDynamic = true;

	/// <summary>
	/// Specify overlay's shape
	/// </summary>
	public OverlayShape currentOverlayShape = OverlayShape.Quad;
	private OverlayShape _prevOverlayShape = OverlayShape.Quad;

	private static Material premultiplyMaterial;

	/// <summary>
	/// Try to avoid setting texture frequently when app is running, texNativePtr updating is slow since rendering thread synchronization
	/// Please cache your nativeTexturePtr and use  OverrideOverlayTextureInfo
	/// </summary>
	public Texture[] textures = new Texture[] { null, null };
	private Texture[][] externalTextures;
	private Texture[] cachedTextures = new Texture[] { null, null };
	private IntPtr[] texNativePtrs = new IntPtr[] { IntPtr.Zero, IntPtr.Zero };
	private OVRPlugin.LayerLayout layout = OVRPlugin.LayerLayout.Mono;
	private int texturesPerStage = 1;

	private int layerIndex = -1; // Controls the composition order based on wake-up time.
	private int layerId = 0; // The layer's internal handle in the compositor.
	private GCHandle layerIdHandle;
	private IntPtr layerIdPtr = IntPtr.Zero;
	OVRPlugin.LayerDesc layerDesc;
	private int frameIndex = 0;
	Renderer rend;

	[HideInInspector]
	public bool isMultiviewEnabled = false;

	/// <summary>
	/// Use this function to set texture and texNativePtr when app is running 
	/// GetNativeTexturePtr is a slow behavior, the value should be pre-cached 
	/// </summary>
	public void OverrideOverlayTextureInfo(Texture srcTexture, IntPtr nativePtr, UnityEngine.XR.XRNode node)
	{
		int index = (node == UnityEngine.XR.XRNode.RightEye) ? 1 : 0;

		if (textures.Length <= index)
			return;
		
		textures[index] = srcTexture;
		cachedTextures[index] = srcTexture;
		texNativePtrs[index] = nativePtr;
	}

	void Awake()
	{
		Debug.Log("Overlay Awake");

		if (premultiplyMaterial == null)
			premultiplyMaterial = new Material(Shader.Find("Oculus/Alpha Premultiply"));

		rend = GetComponent<Renderer>();

		if (textures.Length == 0)
			textures = new Texture[] { null };

		// Backward compatibility
		if (rend != null && textures[0] == null)
			textures[0] = rend.material.mainTexture;

		if (textures[0] != null)
		{
			cachedTextures[0] = textures[0];
			texNativePtrs[0] = textures[0].GetNativeTexturePtr();
		}

#if UNITY_ANDROID && !UNITY_EDITOR
		if (textures.Length == 2 && textures[1] != null)
			layout = (isMultiviewEnabled) ? OVRPlugin.LayerLayout.Array : OVRPlugin.LayerLayout.Stereo;
		texturesPerStage = (layout == OVRPlugin.LayerLayout.Stereo) ? 2 : 1;
#endif
	}

	void OnEnable()
	{
		if (!OVRManager.isHmdPresent)
		{
			enabled = false;
			return;
		}

		OnDisable();

		for (int i = 0; i < maxInstances; ++i)
		{
			if (instances[i] == null || instances[i] == this)
			{
				layerIndex = i;
				instances[i] = this;
				break;
			}
		}

		layerIdHandle = GCHandle.Alloc(layerId, GCHandleType.Pinned);
		layerIdPtr = layerIdHandle.AddrOfPinnedObject();
	}

	void OnDisable()
	{
		if (layerIndex != -1)
		{
			// Turn off the overlay if it was on.
			OVRPlugin.EnqueueSubmitLayer(true, false, IntPtr.Zero, IntPtr.Zero, -1, 0, OVRPose.identity.ToPosef(), Vector3.one.ToVector3f(), layerIndex, (OVRPlugin.OverlayShape)_prevOverlayShape);
			instances[layerIndex] = null;
		}

		if (layerIdPtr != IntPtr.Zero)
		{
			OVRPlugin.EnqueueDestroyLayer(layerIdPtr);
			layerIdPtr = IntPtr.Zero;
			layerIdHandle.Free();
		}

		layerIndex = -1;
	}

	int prevFrameIndex = -1;

	void OnRenderObject()
	{
		// The overlay must be specified every eye frame, because it is positioned relative to the
		// current head location.  If frames are dropped, it will be time warped appropriately,
		// just like the eye buffers.
		if (!Camera.current.CompareTag("MainCamera") || Camera.current.cameraType != CameraType.Game || layerIndex == -1 || currentOverlayType == OverlayType.None || textures.Length < texturesPerStage)
			return;


		// Don't submit the same frame twice.
		if (Time.frameCount <= prevFrameIndex)
			return;
		prevFrameIndex = Time.frameCount;

#if !UNITY_ANDROID || UNITY_EDITOR
		if (currentOverlayShape == OverlayShape.OffcenterCubemap)
		{
			Debug.LogWarning("Overlay shape " + currentOverlayShape + " is not supported on current platform");
		}
#endif

		for (int i = 0; i < texturesPerStage; ++i)
		{
			if (textures[i] != cachedTextures[i])
			{
				cachedTextures[i] = textures[i];
				if (cachedTextures[i] != null)
					texNativePtrs[i] = cachedTextures[i].GetNativeTexturePtr();
			}

			if (currentOverlayShape == OverlayShape.Cubemap)
			{
				if (textures[i] != null && textures[i].GetType() != typeof(Cubemap))
				{
					Debug.LogError("Need Cubemap texture for cube map overlay");
					return;
				}
			}
		}

		if (cachedTextures[0] == null || texNativePtrs[0] == IntPtr.Zero)
			return;

		bool overlay = (currentOverlayType == OverlayType.Overlay);
		bool headLocked = false;
		for (var t = transform; t != null && !headLocked; t = t.parent)
			headLocked |= (t == Camera.current.transform);

		OVRPose pose = (headLocked) ? transform.ToHeadSpacePose() : transform.ToTrackingSpacePose();
		Vector3 scale = transform.lossyScale;
		for (int i = 0; i < 3; ++i)
			scale[i] /= Camera.current.transform.lossyScale[i];
#if !UNITY_ANDROID
		if (currentOverlayShape == OverlayShape.Cubemap)
		{
			pose.position = Camera.current.transform.position;
		}
#endif
		// Pack the offsetCenter directly into pose.position for offcenterCubemap
		if (currentOverlayShape == OverlayShape.OffcenterCubemap)
		{
			pose.position = transform.position;
			if ( pose.position.magnitude > 1.0f )
			{
				Debug.LogWarning("your cube map center offset's magnitude is greater than 1, which will cause some cube map pixel always invisible .");
			}
		}
		// Cylinder overlay sanity checking
		if (currentOverlayShape == OverlayShape.Cylinder)
		{
			float arcAngle = scale.x / scale.z / (float)Math.PI * 180.0f;
			if (arcAngle > 180.0f)
			{
				Debug.LogError("Cylinder overlay's arc angle has to be below 180 degree, current arc angle is " + arcAngle + " degree." );
				return ;
			}
		}

		OVRPlugin.Sizei size = new OVRPlugin.Sizei() { w = textures[0].width, h = textures[0].height };
		int flags = (int)OVRPlugin.LayerFlags.TextureOriginAtBottomLeft;
		int mipLevels = 1;
		int sampleCount = 1;
		TextureFormat txFormat = TextureFormat.BGRA32;
		OVRPlugin.EyeTextureFormat etFormat = OVRPlugin.EyeTextureFormat.B8G8R8A8_sRGB;
		RenderTextureFormat rtFormat = RenderTextureFormat.BGRA32;

		var tex2D = textures[0] as Texture2D;
		if (tex2D != null)
		{
			if (tex2D.format == TextureFormat.RGBAHalf || tex2D.format == TextureFormat.RGBAFloat)
			{
				txFormat = TextureFormat.RGBAHalf;
				etFormat = OVRPlugin.EyeTextureFormat.R16G16B16A16_FP;
				rtFormat = RenderTextureFormat.ARGBHalf;
			}
		}

		var rt = textures[0] as RenderTexture;
		if (rt != null)
		{
			sampleCount = rt.antiAliasing;

			if (rt.format == RenderTextureFormat.ARGBHalf)
			{
				txFormat = TextureFormat.RGBAHalf;
				etFormat = OVRPlugin.EyeTextureFormat.R16G16B16A16_FP;
				rtFormat = RenderTextureFormat.ARGBHalf;
			}
		}

		bool needsSetup = (
			!layerDesc.TextureSize.Equals(size) ||
			layerDesc.SampleCount != sampleCount ||
			layerDesc.LayerFlags != flags ||
			layerDesc.Shape != (OVRPlugin.OverlayShape)currentOverlayShape ||
			layerDesc.Layout != layout ||
			layerDesc.Format != etFormat);

		OVRPlugin.LayerDesc desc = new OVRPlugin.LayerDesc();

		if (layerIdPtr != IntPtr.Zero && needsSetup)
		{
			if ((int)layerIdHandle.Target != 0)
				OVRPlugin.EnqueueDestroyLayer(layerIdPtr);

			desc = OVRPlugin.CalculateLayerDesc((OVRPlugin.OverlayShape)currentOverlayShape, layout, size, mipLevels, sampleCount, etFormat, flags);
			OVRPlugin.EnqueueSetupLayer(desc, layerIdPtr);
			layerId = (int)layerIdHandle.Target;

			if (layerId > 0)
				layerDesc = desc;
		}

		if (layerId > 0)
		{
			// For newer SDKs, blit directly to the surface that will be used in compositing.

			int stageCount = OVRPlugin.GetLayerTextureStageCount(layerId);

			if (externalTextures == null)
			{
				frameIndex = 0;
				externalTextures = new Texture[texturesPerStage][];
			}
			
			for (int eyeId = 0; eyeId < texturesPerStage; ++eyeId)
			{
				if (externalTextures[eyeId] == null)
					externalTextures[eyeId] = new Texture[stageCount];

				int stage = frameIndex % stageCount;

				IntPtr externalTex = OVRPlugin.GetLayerTexture(layerId, stage, (OVRPlugin.Eye)eyeId);

				if (externalTex == IntPtr.Zero)
					continue;

				bool needsCopy = isDynamic;

				Texture et = externalTextures[eyeId][stage];
				if (et == null)
				{
					bool isSrgb = (etFormat == OVRPlugin.EyeTextureFormat.B8G8R8A8_sRGB || etFormat == OVRPlugin.EyeTextureFormat.R8G8B8A8_sRGB);

					if (currentOverlayShape != OverlayShape.Cubemap && currentOverlayShape != OverlayShape.OffcenterCubemap)
						et = Texture2D.CreateExternalTexture(size.w, size.h, txFormat, mipLevels > 1, isSrgb, externalTex);
#if UNITY_2017_1_OR_NEWER
					else
						et = Cubemap.CreateExternalTexture(size.w, txFormat, mipLevels > 1, externalTex);
#endif
					
					externalTextures[eyeId][stage] = et;
					needsCopy = true;
				}

				if (needsCopy)
				{
					// The compositor uses premultiplied alpha, so multiply it here.
					if (currentOverlayShape != OverlayShape.Cubemap && currentOverlayShape != OverlayShape.OffcenterCubemap)
					{
						var tempRT = RenderTexture.GetTemporary(size.w, size.h, 0, rtFormat, RenderTextureReadWrite.Default, sampleCount);
#if UNITY_ANDROID && !UNITY_EDITOR
						Graphics.Blit(textures[eyeId], tempRT); //Resolve, decompress, swizzle, etc not handled by simple CopyTexture.
#else
						Graphics.Blit(textures[eyeId], tempRT, premultiplyMaterial);
#endif
						Graphics.CopyTexture(tempRT, 0, 0, et, 0, 0);
						RenderTexture.ReleaseTemporary(tempRT);
					}
#if UNITY_2017_1_OR_NEWER
					else
					{
						var tempRTSrc = RenderTexture.GetTemporary(size.w, size.h, 0, rtFormat, RenderTextureReadWrite.Default, sampleCount);
						var tempRTDst = RenderTexture.GetTemporary(size.w, size.h, 0, rtFormat, RenderTextureReadWrite.Default, sampleCount);

						for (int face = 0; face < 6; ++face)
						{
							//HACK: It would be much more efficient to blit directly from textures[eyeId] to et, but Unity's API doesn't support that.
							//Suggest using a native plugin to render directly to a cubemap layer for 360 video, etc.
							Graphics.CopyTexture(textures[eyeId], face, 0, tempRTSrc, 0, 0);
							Graphics.Blit(tempRTSrc, tempRTDst, premultiplyMaterial);
							Graphics.CopyTexture(tempRTDst, 0, 0, et, face, 0);
						}
						RenderTexture.ReleaseTemporary(tempRTSrc);
						RenderTexture.ReleaseTemporary(tempRTDst);
					}
#endif
				}
			}

			bool isOverlayVisible = OVRPlugin.EnqueueSubmitLayer(overlay, headLocked, texNativePtrs[0], texNativePtrs[1], layerId, frameIndex, pose.flipZ().ToPosef(), scale.ToVector3f(), layerIndex, (OVRPlugin.OverlayShape)currentOverlayShape);
			if (isDynamic)
				++frameIndex;
			_prevOverlayShape = currentOverlayShape;
			if (rend)
				rend.enabled = !isOverlayVisible;
		}
	}

}
