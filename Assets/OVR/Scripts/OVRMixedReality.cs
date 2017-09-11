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
using System;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using UnityEngine;
using VR = UnityEngine.VR;

/// <summary>
/// Manages mix-reality elements
/// </summary>
internal static class OVRMixedReality
{
	/// <summary>
	/// Configurable parameters
	/// </summary>
	public static Color chromaKeyColor = Color.green;

	/// <summary>
	/// For Debugging purpose, we can use preset parameters to fake a camera when external camera is not available
	/// </summary>
	private static bool useFakeExternalCamera = false;
	private static Vector3 fakeCameraPositon = new Vector3(3.0f, 0.0f, 3.0f);
	private static Quaternion fakeCameraRotation = Quaternion.LookRotation((new Vector3(0.0f, 1.0f, 0.0f) - fakeCameraPositon).normalized, Vector3.up);
	private static float fakeCameraFov = 60.0f;
	private static float fakeCameraAspect = 16.0f / 9.0f;

	/// <summary>
	/// Mix-reality supporting gameObjects, only created when mix-reality mode is on in the first Update()
	/// </summary>

	// external composition
	public static bool externalCompositionObjectsInitialized = false;
	private static GameObject foregroundCameraGameObject;
	private static Camera foregroundCamera;
	private static GameObject backgroundCameraGameObject;
	private static Camera backgroundCamera;
	private static GameObject cameraProxyPlane;

	// direct composition
	public static bool directCompositionObjectsInitialized = false;
	private static GameObject directCompositionCameraGameObject;
	private static Camera directCompositionCamera;
	private static GameObject cameraFramePlaneObject;
	public static bool directCompositionCameraDeviceOpened = false;
	private static OVRPlugin.CameraDevice directCompositionCameraDevice = OVRPlugin.CameraDevice.WebCamera0;
	public static float directCompositionChromaAlphaCutoff = 0.01f;
	public static float directCompositionChromaToleranceA = 5.0f;
	public static float directCompositionChromaToleranceB = 5.0f;
	public static float directCompositionChromaShadows = 0.02f;

	/// <summary>
	/// Updates the internal state of the Mixed Reality Camera. Called by OVRManager.
	/// </summary>

	public static void Update(GameObject parentObject, Camera mainCamera, bool useDirectComposition)
	{
		if (!OVRPlugin.IsMixedRealityInitialized())
			OVRPlugin.InitializeMixedReality();

		Debug.Assert(OVRPlugin.IsMixedRealityInitialized());
		OVRPlugin.UpdateExternalCamera();
		OVRPlugin.UpdateCameraDevices();

		//if (OVRPlugin.GetExternalCameraCount() > 0 || useFakeExternalCamera)
		{
			if (!useDirectComposition)
			{
				if (directCompositionObjectsInitialized)
				{
					CleanupDirectCompositionObjects();
				}
				// Create background / foreground camera and set them to output onto mirror window side by side for compositing software
				if (!externalCompositionObjectsInitialized)
				{
					InitializeExternalCompositionObjects(parentObject, mainCamera);
				}
				UpdateExternalCompositionObjects(mainCamera);
			}
			else
			{
				if (externalCompositionObjectsInitialized)
				{
					CleanupExternalCompositionObjects();
				}
				// Create background / foreground camera and set them to output onto mirror window side by side for compositing software
				if (!directCompositionObjectsInitialized)
				{
					InitializeDirectCompositionObjects(parentObject, mainCamera);
				}
				UpdateDirectCompositionObjects(mainCamera);
			}
		}
	}

	public static void Cleanup()
	{
		if (directCompositionObjectsInitialized)
		{
			CleanupDirectCompositionObjects();
		}
		if (externalCompositionObjectsInitialized)
		{
			CleanupExternalCompositionObjects();
		}
		OVRPlugin.ShutdownMixedReality();
	}

	private static void InitializeExternalCompositionObjects(GameObject parentObject, Camera mainCamera)
	{
		Debug.Assert(!externalCompositionObjectsInitialized);
		Debug.Assert(!directCompositionObjectsInitialized);

		Debug.Assert(backgroundCameraGameObject == null);
		backgroundCameraGameObject = new GameObject();
		backgroundCameraGameObject.name = "MRBackgroundCamera";
		backgroundCameraGameObject.transform.parent = parentObject.transform;
		backgroundCamera = backgroundCameraGameObject.AddComponent<Camera>();
		backgroundCamera.stereoTargetEye = StereoTargetEyeMask.None;
		backgroundCamera.depth = float.MaxValue;
		backgroundCamera.rect = new Rect(0.0f, 0.0f, 0.5f, 1.0f);
		backgroundCamera.clearFlags = mainCamera.clearFlags;
		backgroundCamera.backgroundColor = mainCamera.backgroundColor;
		backgroundCamera.cullingMask = mainCamera.cullingMask;
		backgroundCamera.nearClipPlane = mainCamera.nearClipPlane;
		backgroundCamera.farClipPlane = mainCamera.farClipPlane;

		Debug.Assert(foregroundCameraGameObject == null);
		foregroundCameraGameObject = new GameObject();
		foregroundCameraGameObject.name = "MRForgroundCamera";
		foregroundCameraGameObject.transform.parent = parentObject.transform;
		foregroundCamera = foregroundCameraGameObject.AddComponent<Camera>();
		foregroundCamera.stereoTargetEye = StereoTargetEyeMask.None;
		foregroundCamera.depth = float.MaxValue;
		foregroundCamera.rect = new Rect(0.5f, 0.0f, 0.5f, 1.0f);
		foregroundCamera.clearFlags = CameraClearFlags.Color;
		foregroundCamera.backgroundColor = chromaKeyColor;
		foregroundCamera.cullingMask = mainCamera.cullingMask;
		foregroundCamera.nearClipPlane = mainCamera.nearClipPlane;
		foregroundCamera.farClipPlane = mainCamera.farClipPlane;

		// Create cameraProxyPlane for clipping
		Debug.Assert(cameraProxyPlane == null);
		cameraProxyPlane = GameObject.CreatePrimitive(PrimitiveType.Quad);
		cameraProxyPlane.name = "MRProxyClipPlane";
		cameraProxyPlane.transform.parent = parentObject.transform;
		cameraProxyPlane.GetComponent<Collider>().enabled = false;
		cameraProxyPlane.GetComponent<MeshRenderer>().shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
		Material clipMaterial = new Material(Shader.Find("Unlit/OVRMRClipPlane"));
		cameraProxyPlane.GetComponent<MeshRenderer>().material = clipMaterial;
		clipMaterial.SetColor("_Color", chromaKeyColor);
		clipMaterial.SetFloat("_Visible", 0.0f);
		cameraProxyPlane.transform.localScale = new Vector3(1000, 1000, 1000);
		cameraProxyPlane.SetActive(true);
		OVRMRForegroundCameraManager foregroundCameraManager = foregroundCameraGameObject.AddComponent<OVRMRForegroundCameraManager>();
		foregroundCameraManager.clipPlaneGameObj = cameraProxyPlane;

		externalCompositionObjectsInitialized = true;
	}

	private static void CleanupExternalCompositionObjects()
	{
		Debug.Assert(externalCompositionObjectsInitialized);

		if (Application.isEditor)
		{
			GameObject.DestroyImmediate(backgroundCameraGameObject);
		}
		else
		{
			GameObject.Destroy(backgroundCameraGameObject);
		}
		backgroundCameraGameObject = null;
		backgroundCamera = null;
		if (Application.isEditor)
		{
			GameObject.DestroyImmediate(foregroundCameraGameObject);
		}
		else
		{
			GameObject.Destroy(foregroundCameraGameObject);
		}
		foregroundCameraGameObject = null;
		foregroundCamera = null;
		if (Application.isEditor)
		{
			GameObject.DestroyImmediate(cameraProxyPlane);
		}
		else
		{
			GameObject.Destroy(cameraProxyPlane);
		}
		cameraProxyPlane = null;

		externalCompositionObjectsInitialized = false;
	}

	private static void UpdateExternalCompositionObjects(Camera mainCamera)
	{
		backgroundCamera.clearFlags = mainCamera.clearFlags;
		backgroundCamera.backgroundColor = mainCamera.backgroundColor;
		backgroundCamera.cullingMask = mainCamera.cullingMask;
		backgroundCamera.nearClipPlane = mainCamera.nearClipPlane;
		backgroundCamera.farClipPlane = mainCamera.farClipPlane;

		foregroundCamera.cullingMask = mainCamera.cullingMask;
		foregroundCamera.nearClipPlane = mainCamera.nearClipPlane;
		foregroundCamera.farClipPlane = mainCamera.farClipPlane;

		if (useFakeExternalCamera || OVRPlugin.GetExternalCameraCount() == 0)
		{
			OVRPose worldSpacePose = new OVRPose();
			OVRPose trackingSpacePose = new OVRPose();
			trackingSpacePose.position = fakeCameraPositon;
			trackingSpacePose.orientation = fakeCameraRotation;
			worldSpacePose = OVRExtensions.ToWorldSpacePose(trackingSpacePose);

			backgroundCamera.fieldOfView = fakeCameraFov;
			backgroundCamera.aspect = fakeCameraAspect;
			backgroundCamera.transform.FromOVRPose(worldSpacePose);

			foregroundCamera.fieldOfView = fakeCameraFov;
			foregroundCamera.aspect = fakeCameraAspect;
			foregroundCamera.transform.FromOVRPose(worldSpacePose);
		}
		else
		{
			OVRPlugin.CameraExtrinsics extrinsics;
			OVRPlugin.CameraIntrinsics intrinsics;

			// So far, only support 1 camera for MR and always use camera index 0
			if (OVRPlugin.GetMixedRealityCameraInfo(0, out extrinsics, out intrinsics))
			{
				OVRPose worldSpacePose = new OVRPose();
				OVRPose trackingSpacePose = new OVRPose();

				OVRPose cameraTrackingSpacePose = extrinsics.RelativePose.ToOVRPose();
				trackingSpacePose = cameraTrackingSpacePose;

				if (OVRPlugin.GetNodePresent(extrinsics.AttachedToNode))
				{
					OVRPose attachedNodePose = OVRPlugin.GetNodePose(extrinsics.AttachedToNode, OVRPlugin.Step.Render).ToOVRPose();
					trackingSpacePose = attachedNodePose * trackingSpacePose;
				}

				worldSpacePose = OVRExtensions.ToWorldSpacePose(trackingSpacePose);

				float fovY = Mathf.Atan(intrinsics.FOVPort.UpTan) * Mathf.Rad2Deg * 2;
				float aspect = intrinsics.FOVPort.LeftTan / intrinsics.FOVPort.UpTan;
				backgroundCamera.fieldOfView = fovY;
				backgroundCamera.aspect = aspect;
				backgroundCamera.transform.FromOVRPose(worldSpacePose);
				foregroundCamera.fieldOfView = fovY;
				foregroundCamera.aspect = intrinsics.FOVPort.LeftTan / intrinsics.FOVPort.UpTan;
				foregroundCamera.transform.FromOVRPose(worldSpacePose);
			}
			else
			{
				Debug.LogError("Failed to get external camera information");
				return;
			}
		}

		// Assume player always standing straightly
		Vector3 externalCameraToHeadXZ = mainCamera.transform.position - foregroundCamera.transform.position;
		externalCameraToHeadXZ.y = 0;
		cameraProxyPlane.transform.position = mainCamera.transform.position;
		cameraProxyPlane.transform.LookAt(cameraProxyPlane.transform.position + externalCameraToHeadXZ);
	}

	private static void InitializeDirectCompositionObjects(GameObject parentObject, Camera mainCamera)
	{
		Debug.Assert(!externalCompositionObjectsInitialized);
		Debug.Assert(!directCompositionObjectsInitialized);

		Debug.Assert(directCompositionCameraGameObject == null);
		directCompositionCameraGameObject = new GameObject();
		directCompositionCameraGameObject.name = "MRDirectCompositionCamera";
		directCompositionCameraGameObject.transform.parent = parentObject.transform;
		directCompositionCamera = directCompositionCameraGameObject.AddComponent<Camera>();
		directCompositionCamera.stereoTargetEye = StereoTargetEyeMask.None;
		directCompositionCamera.depth = float.MaxValue;
		directCompositionCamera.rect = new Rect(0.0f, 0.0f, 1.0f, 1.0f);
		directCompositionCamera.clearFlags = mainCamera.clearFlags;
		directCompositionCamera.backgroundColor = mainCamera.backgroundColor;
		directCompositionCamera.cullingMask = mainCamera.cullingMask;
		directCompositionCamera.nearClipPlane = mainCamera.nearClipPlane;
		directCompositionCamera.farClipPlane = mainCamera.farClipPlane;

		Debug.Assert(cameraFramePlaneObject == null);
		cameraFramePlaneObject = GameObject.CreatePrimitive(PrimitiveType.Quad);
		cameraFramePlaneObject.name = "MRCameraFrame";
		cameraFramePlaneObject.transform.parent = parentObject.transform;
		cameraFramePlaneObject.GetComponent<Collider>().enabled = false;
		cameraFramePlaneObject.GetComponent<MeshRenderer>().shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
		Material cameraFrameMaterial = new Material(Shader.Find("Unlit/OVRMRCameraFrame"));
		cameraFramePlaneObject.GetComponent<MeshRenderer>().material = cameraFrameMaterial;
		cameraFrameMaterial.SetColor("_Color", Color.white);
		cameraFrameMaterial.SetFloat("_Visible", 0.0f);
		cameraFramePlaneObject.transform.localScale = new Vector3(4, 4, 4);
		cameraFramePlaneObject.SetActive(true);
		OVRMRDirectCompositionCameraManager directCompositionCameraManager = directCompositionCameraGameObject.AddComponent<OVRMRDirectCompositionCameraManager>();
		directCompositionCameraManager.cameraFrameGameObj = cameraFramePlaneObject;

		Debug.Assert(!directCompositionCameraDeviceOpened);
		Debug.Assert(!OVRPlugin.HasCameraDeviceOpened(directCompositionCameraDevice));
		if (OVRPlugin.IsCameraDeviceAvailable(directCompositionCameraDevice))
		{
			OVRPlugin.CameraExtrinsics extrinsics;
			OVRPlugin.CameraIntrinsics intrinsics;
			if (OVRPlugin.GetExternalCameraCount() > 0 && OVRPlugin.GetMixedRealityCameraInfo(0, out extrinsics, out intrinsics))
			{
				OVRPlugin.SetCameraDevicePreferredColorFrameSize(directCompositionCameraDevice, intrinsics.ImageSensorPixelResolution.w, intrinsics.ImageSensorPixelResolution.h);
			}

			OVRPlugin.OpenCameraDevice(directCompositionCameraDevice);
			if (OVRPlugin.HasCameraDeviceOpened(directCompositionCameraDevice))
			{
				directCompositionCameraDeviceOpened = true;
			}
		}

		directCompositionObjectsInitialized = true;
	}

	private static void CleanupDirectCompositionObjects()
	{
		Debug.Assert(directCompositionObjectsInitialized);

		if (Application.isEditor)
		{
			GameObject.DestroyImmediate(directCompositionCameraGameObject);
		}
		else
		{
			GameObject.Destroy(directCompositionCameraGameObject);
		}
		directCompositionCameraGameObject = null;
		directCompositionCamera = null;

		if (Application.isEditor)
		{
			GameObject.DestroyImmediate(cameraFramePlaneObject);
		}
		else
		{
			GameObject.Destroy(cameraFramePlaneObject);
		}

		if (directCompositionCameraDeviceOpened)
		{
			OVRPlugin.CloseCameraDevice(directCompositionCameraDevice);
		}

		directCompositionObjectsInitialized = false;
	}

	private static void UpdateDirectCompositionObjects(Camera mainCamera)
	{
		directCompositionCamera.clearFlags = mainCamera.clearFlags;
		directCompositionCamera.backgroundColor = mainCamera.backgroundColor;
		directCompositionCamera.cullingMask = mainCamera.cullingMask;
		directCompositionCamera.nearClipPlane = mainCamera.nearClipPlane;
		directCompositionCamera.farClipPlane = mainCamera.farClipPlane;

		if (useFakeExternalCamera || OVRPlugin.GetExternalCameraCount() == 0)
		{
			OVRPose worldSpacePose = new OVRPose();
			OVRPose trackingSpacePose = new OVRPose();
			trackingSpacePose.position = fakeCameraPositon;
			trackingSpacePose.orientation = fakeCameraRotation;
			worldSpacePose = OVRExtensions.ToWorldSpacePose(trackingSpacePose);

			directCompositionCamera.fieldOfView = fakeCameraFov;
			directCompositionCamera.aspect = fakeCameraAspect;
			directCompositionCamera.transform.FromOVRPose(worldSpacePose);
		}
		else
		{
			OVRPlugin.CameraExtrinsics extrinsics;
			OVRPlugin.CameraIntrinsics intrinsics;

			// So far, only support 1 camera for MR and always use camera index 0
			if (OVRPlugin.GetMixedRealityCameraInfo(0, out extrinsics, out intrinsics))
			{
				OVRPose worldSpacePose = new OVRPose();
				OVRPose trackingSpacePose = new OVRPose();

				OVRPose cameraTrackingSpacePose = extrinsics.RelativePose.ToOVRPose();
				trackingSpacePose = cameraTrackingSpacePose;

				if (OVRPlugin.GetNodePresent(extrinsics.AttachedToNode))
				{
					OVRPose attachedNodePose = OVRPlugin.GetNodePose(extrinsics.AttachedToNode, OVRPlugin.Step.Render).ToOVRPose();
					trackingSpacePose = attachedNodePose * trackingSpacePose;
				}

				worldSpacePose = OVRExtensions.ToWorldSpacePose(trackingSpacePose);

				float fovY = Mathf.Atan(intrinsics.FOVPort.UpTan) * Mathf.Rad2Deg * 2;
				float aspect = intrinsics.FOVPort.LeftTan / intrinsics.FOVPort.UpTan;
				directCompositionCamera.fieldOfView = fovY;
				directCompositionCamera.aspect = aspect;
				directCompositionCamera.transform.FromOVRPose(worldSpacePose);
			}
			else
			{
				Debug.LogWarning("Failed to get external camera information");
			}
		}

		if (OVRPlugin.HasCameraDeviceOpened(directCompositionCameraDevice))
		{
			if (OVRPlugin.IsCameraDeviceColorFrameAvailable(directCompositionCameraDevice))
			{
				cameraFramePlaneObject.GetComponent<MeshRenderer>().material.mainTexture = OVRPlugin.GetCameraDeviceColorFrameTexture(directCompositionCameraDevice);
			}
		}

		Vector3 offset = mainCamera.transform.position - directCompositionCameraGameObject.transform.position;
		float distance = Vector3.Dot(directCompositionCameraGameObject.transform.forward, offset);

		cameraFramePlaneObject.transform.position = directCompositionCameraGameObject.transform.position + directCompositionCameraGameObject.transform.forward * distance;
		cameraFramePlaneObject.transform.rotation = directCompositionCameraGameObject.transform.rotation;

		float tanFov = Mathf.Tan(directCompositionCamera.fieldOfView * Mathf.Deg2Rad * 0.5f);
		cameraFramePlaneObject.transform.localScale = new Vector3(distance * directCompositionCamera.aspect * tanFov * 2.0f, distance * tanFov * 2.0f, 1.0f);
	}

}


/// <summary>
/// Helper internal class for foregroundCamera, don't call it outside
/// </summary>
internal class OVRMRForegroundCameraManager : MonoBehaviour
{
	public GameObject clipPlaneGameObj;
	private Material clipPlaneMaterial;
	void OnPreRender()
	{
		// the clipPlaneGameObj should be only visible to foreground camera
		if (clipPlaneGameObj)
		{
			if (clipPlaneMaterial == null)
				clipPlaneMaterial = clipPlaneGameObj.GetComponent<MeshRenderer>().material;
			clipPlaneGameObj.GetComponent<MeshRenderer>().material.SetFloat("_Visible", 1.0f);
		}
	}
	void OnPostRender()
	{
		if (clipPlaneGameObj)
		{
			Debug.Assert(clipPlaneMaterial);
			clipPlaneGameObj.GetComponent<MeshRenderer>().material.SetFloat("_Visible", 0.0f);
		}
	}
}

/// <summary>
/// Helper internal class for DirectCompositionCamera, don't call it outside
/// </summary>
internal class OVRMRDirectCompositionCameraManager : MonoBehaviour
{
	public GameObject cameraFrameGameObj;
	private Material cameraFrameMaterial;
	void OnPreRender()
	{
		// the clipPlaneGameObj should be only visible to foreground camera
		if (cameraFrameGameObj)
		{
			if (cameraFrameMaterial == null)
				cameraFrameMaterial = cameraFrameGameObj.GetComponent<MeshRenderer>().material;
			cameraFrameMaterial.SetFloat("_Visible", 1.0f);
			cameraFrameMaterial.SetFloat("_ChromaAlphaCutoff", OVRMixedReality.directCompositionChromaAlphaCutoff);
			cameraFrameMaterial.SetFloat("_ChromaToleranceA", OVRMixedReality.directCompositionChromaToleranceA);
			cameraFrameMaterial.SetFloat("_ChromaToleranceB", OVRMixedReality.directCompositionChromaToleranceB);
			cameraFrameMaterial.SetFloat("_ChromaShadows", OVRMixedReality.directCompositionChromaShadows);
		}
	}
	void OnPostRender()
	{
		if (cameraFrameGameObj)
		{
			Debug.Assert(cameraFrameMaterial);
			cameraFrameMaterial.SetFloat("_Visible", 0.0f);
		}
	}
}