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

#if UNITY_EDITOR

using UnityEngine;
using UnityEditor;
using System.Reflection;
using System;
using System.Collections;
using System.Collections.Generic;

/// <summary>
///Scans the project and warns about the following conditions:
///Audio sources > 16
///Using MSAA levels other than recommended level
///GPU skinning is also probably usually ideal.
///Excessive pixel lights (>1 on Gear VR; >3 on Rift)
///Directional Lightmapping Modes (on Gear; use Non-Directional)
///Preload audio setting on individual audio clips
///Decompressing audio clips on load
///Disabling occlusion mesh
///Android target API level set to 19 or higher
///Unity skybox use (on by default, but if you can't see the skybox switching to Color is much faster on Gear)
///Lights marked as "baked" but that were not included in the last bake (and are therefore realtime).
///Lack of static batching and dynamic batching settings activated.
///Full screen image effects (Gear)
///Warn about large textures that are marked as uncompressed.
///32-bit depth buffer (use 16)
///Use of projectors (Gear; can be used carefully but slow enough to warrant a warning)
///Maybe in the future once quantified: Graphics jobs and IL2CPP on Gear.
///Real-time global illumination
///No texture compression, or non-ASTC texture compression as a global setting (Gear).
///Using deferred rendering
///Excessive texture resolution after LOD bias (>2k on Gear VR; >4k on Rift)
///Not using trilinear or aniso filtering and not generating mipmaps
///Excessive render scale (>1.2)
///Slow physics settings: Sleep Threshold < 0.005, Default Contact Offset < 0.01, Solver Iteration Count > 6
///Shadows on when approaching the geometry or draw call limits
///Non-static objects with colliders that are missing rigidbodies on themselves or in the parent chain.
///No initialization of GPU/CPU throttling settings, or init to dangerous values (-1 or > 3)  (Gear)
///Using inefficient effects: SSAO, motion blur, global fog, parallax mapping, etc.
///Too many Overlay layers
///Use of Standard shader or Standard Specular shader on Gear.  More generally, excessive use of multipass shaders (legacy specular, etc).
///Multiple cameras with clears (on Gear, potential for excessive fill cost)
///Excessive shader passes (>2)
///Material pointers that have been instanced in the editor (esp. if we could determine that the instance has no deltas from the original)
///Excessive draw calls (>150 on Gear VR; >2000 on Rift)
///Excessive tris or verts (>100k on Gear VR; >1M on Rift)
///Large textures, lots of prefabs in startup scene (for bootstrap optimization)
/// </summary>
public class OVRLint : EditorWindow
{
	//TODO: The following require reflection or static analysis.
	///Use of ONSP reflections (Gear)
	///Use of LoadLevelAsync / LoadLevelAdditiveAsync (on Gear, this kills frame rate so dramatically it's probably better to just go to black and load synchronously)
	///Use of Linq in non-editor assemblies (common cause of GCs).  Minor: use of foreach.
	///Use of Unity WWW (exceptionally high overhead for large file downloads, but acceptable for tiny gets).
	///Declared but empty Awake/Start/Update/OnCollisionEnter/OnCollisionExit/OnCollisionStay.  Also OnCollision* star methods that declare the Collision  argument but do not reference it (omitting it short-circuits the collision contact calculation).

	public delegate void FixMethodDelegate(UnityEngine.Object obj, bool isLastInSet, int selectedIndex);
	
	public struct FixRecord
	{
		public string category;
		public string message;
		public FixMethodDelegate fixMethod;
		public UnityEngine.Object targetObject;
		public string[] buttonNames;
		public bool complete;
		
		
		public FixRecord(string cat, string msg, FixMethodDelegate fix, UnityEngine.Object target, string[] buttons)
		{
			category = cat;
			message = msg;
			buttonNames = buttons;
			fixMethod = fix;
			targetObject = target;
			complete = false;
		}
	}
	
	private static List<FixRecord> mRecords = new List<FixRecord>();
	private Vector2 mScrollPosition;

	
	[MenuItem("Tools/Oculus/Audit Project for VR Performance Issues")]
	static void Init () 
    {
        // Get existing open window or if none, make a new one:
       	EditorWindow.GetWindow (typeof (OVRLint));
    }
    
    void OnGUI () 
    {
    	GUILayout.Label ("OVR Lint Tool", EditorStyles.boldLabel);
    	if (GUILayout.Button("Run Lint", EditorStyles.toolbarButton, GUILayout.ExpandWidth(false)))
		{
			RunCheck();
		}
		
		string lastCategory = "";
		
		mScrollPosition = EditorGUILayout.BeginScrollView(mScrollPosition);
		
		for (int x = 0; x < mRecords.Count; x++)
		{
			FixRecord record = mRecords[x];
			
			if (!record.category.Equals(lastCategory))	// new category
			{
				lastCategory = record.category;
				EditorGUILayout.Separator();
				EditorGUILayout.BeginHorizontal();
    			GUILayout.Label(lastCategory, EditorStyles.label, GUILayout.Width(200));
    			bool moreThanOne = (x + 1 < mRecords.Count && mRecords[x + 1].category.Equals(lastCategory));
    			if (record.buttonNames != null && record.buttonNames.Length > 0)
    			{
    				if (moreThanOne)
    				{
						GUILayout.Label("Apply to all:", EditorStyles.label, GUILayout.Width(75));
						for (int y = 0; y < record.buttonNames.Length; y++)
						{
							if (GUILayout.Button(record.buttonNames[y], EditorStyles.toolbarButton, GUILayout.Width(100)))
							{
								List<FixRecord> recordsToProcess = new List<FixRecord>();
								
								for (int z = x; z < mRecords.Count; z++)
								{
									FixRecord thisRecord = mRecords[z];
									bool isLast = false;
									if (z + 1 >= mRecords.Count || !mRecords[z + 1].category.Equals(lastCategory))
									{
										isLast = true;
									}
									
									if (!thisRecord.complete)
									{
										recordsToProcess.Add(thisRecord);
									}
									
									if (isLast)
									{
										break;
									}
								}	
								
								UnityEngine.Object[] undoObjects = new UnityEngine.Object[recordsToProcess.Count];
								for (int z = 0; z < recordsToProcess.Count; z++)
								{
									undoObjects[z] = recordsToProcess[z].targetObject;
								}
								Undo.RecordObjects(undoObjects, record.category + " (Multiple)");
								for (int z = 0; z < recordsToProcess.Count; z++)
								{
									FixRecord thisRecord = recordsToProcess[z];
									thisRecord.fixMethod(thisRecord.targetObject, (z + 1 == recordsToProcess.Count), y);
									thisRecord.complete = true;
								}
							}
						}
					}
    			}
    			EditorGUILayout.EndHorizontal();
    			if (moreThanOne || record.targetObject)
    			{
    				GUILayout.Label(record.message);
           	 	}
			}
			
			EditorGUILayout.BeginHorizontal();
			GUI.enabled = !record.complete;
			if (record.targetObject)
			{
				EditorGUILayout.ObjectField(record.targetObject, record.targetObject.GetType(), true);
			}
			else
			{
				GUILayout.Label(record.message);
			}
			for (int y = 0; y < record.buttonNames.Length; y++)
			{
				if (GUILayout.Button(record.buttonNames[y], EditorStyles.toolbarButton, GUILayout.Width(100)))
				{
					if (record.targetObject != null)
					{
						Undo.RecordObject(record.targetObject, record.category);
					}
					record.fixMethod(record.targetObject, true, y);
					record.complete = true;
				}
			}
			GUI.enabled = true;
			EditorGUILayout.EndHorizontal();
		}
		
		EditorGUILayout.EndScrollView();
    }
    
    
	static void RunCheck()
	{
		mRecords.Clear();
		
		CheckStaticCommonIssues();
		#if UNITY_ANDROID
		CheckStaticAndroidIssues();
		#endif

		if (EditorApplication.isPlaying)
		{
			CheckRuntimeCommonIssues();
			#if UNITY_ANDROID
			CheckRuntimeAndroidIssues();
			#endif
		}
		
		mRecords.Sort(delegate(FixRecord record1, FixRecord record2)
		{
			return record1.category.CompareTo(record2.category);
		});
	}
	
	static void AddFix(string category, string message, FixMethodDelegate method, UnityEngine.Object target, params string[] buttons)
	{
		mRecords.Add(new FixRecord(category, message, method, target, buttons));
	}

	static void CheckStaticCommonIssues ()
	{
		if (QualitySettings.anisotropicFiltering != AnisotropicFiltering.Enable)
		{
			AddFix("Optimize Aniso", "Anisotropic filtering is recommended for optimal quality and performance.", delegate(UnityEngine.Object obj, bool last, int selected) 
			{
				QualitySettings.anisotropicFiltering = AnisotropicFiltering.Enable;
			}, null, "Fix");
		}

		#if UNITY_ANDROID
		int recommendedPixelLightCount = 1;
		#else
		int recommendedPixelLightCount = 3;
		#endif

		if (QualitySettings.pixelLightCount > recommendedPixelLightCount)
		{
			AddFix("Optimize Pixel Light Count", "For GPU performance set no more than " + recommendedPixelLightCount + " pixel lights in Quality Settings (currently " + QualitySettings.pixelLightCount + ").", delegate(UnityEngine.Object obj, bool last, int selected) 
			{
				QualitySettings.pixelLightCount = recommendedPixelLightCount;
			}, null, "Fix");
		}

		if (!PlayerSettings.gpuSkinning)
		{
		    AddFix ("Optimize GPU Skinning", "For CPU performance, please use GPU skinning.", delegate(UnityEngine.Object obj, bool last, int selected) 
		    {
				PlayerSettings.gpuSkinning = true;
			}, null, "Fix");
		}

#if UNITY_5_4_OR_NEWER
		// Should we recommend this?  Seems to be mutually exclusive w/ dynamic batching.
		if (!PlayerSettings.graphicsJobs)
		{
			AddFix ("Optimize Graphics Jobs", "For CPU performance, please use graphics jobs.", delegate(UnityEngine.Object obj, bool last, int selected) 
			{
				PlayerSettings.graphicsJobs = true;
			}, null, "Fix");
		}
#endif

#if UNITY_2017_2_OR_NEWER
		if ((!PlayerSettings.MTRendering || !PlayerSettings.GetMobileMTRendering(BuildTargetGroup.Android)))
#else
		if ((!PlayerSettings.MTRendering || !PlayerSettings.mobileMTRendering))
#endif
		{
		    AddFix ("Optimize MT Rendering", "For CPU performance, please enable multithreaded rendering.", delegate(UnityEngine.Object obj, bool last, int selected)
		    {
#if UNITY_2017_2_OR_NEWER
				PlayerSettings.SetMobileMTRendering(BuildTargetGroup.Standalone, true);
				PlayerSettings.SetMobileMTRendering(BuildTargetGroup.Android, true);
#else
				PlayerSettings.MTRendering = PlayerSettings.mobileMTRendering = true;
#endif
			}, null, "Fix");
		}

#if UNITY_5_5_OR_NEWER
		BuildTargetGroup target = EditorUserBuildSettings.selectedBuildTargetGroup;
		var tier = UnityEngine.Rendering.GraphicsTier.Tier1;
		var tierSettings = UnityEditor.Rendering.EditorGraphicsSettings.GetTierSettings(target, tier);

		if ((tierSettings.renderingPath == RenderingPath.DeferredShading ||
			tierSettings.renderingPath == RenderingPath.DeferredLighting))
		{
		    AddFix ("Optimize Rendering Path", "For CPU performance, please do not use deferred shading.", delegate(UnityEngine.Object obj, bool last, int selected) 
		    {
				tierSettings.renderingPath = RenderingPath.Forward;
				UnityEditor.Rendering.EditorGraphicsSettings.SetTierSettings(target, tier, tierSettings);
			}, null, "Use Forward");
		}
#else
		if (PlayerSettings.renderingPath == RenderingPath.DeferredShading ||
		    PlayerSettings.renderingPath == RenderingPath.DeferredLighting ||
		    PlayerSettings.mobileRenderingPath == RenderingPath.DeferredShading ||
		    PlayerSettings.mobileRenderingPath == RenderingPath.DeferredLighting)
		{
		    AddFix ("Optimize Rendering Path", "For CPU performance, please do not use deferred shading.", delegate(UnityEngine.Object obj, bool last, int selected) 
		    {
				PlayerSettings.renderingPath = PlayerSettings.mobileRenderingPath = RenderingPath.Forward;
			}, null, "Use Forward");
		}
#endif

#if UNITY_5_5_OR_NEWER
		if (PlayerSettings.stereoRenderingPath == StereoRenderingPath.MultiPass)
		{
		    AddFix ("Optimize Stereo Rendering", "For CPU performance, please enable single-pass or instanced stereo rendering.", delegate(UnityEngine.Object obj, bool last, int selected) 
		    {
				PlayerSettings.stereoRenderingPath = StereoRenderingPath.Instancing;
			}, null, "Fix");
		}
#elif UNITY_5_4_OR_NEWER
		if (!PlayerSettings.singlePassStereoRendering)
		{
			AddFix ("Optimize Stereo Rendering", "For CPU performance, please enable single-pass or instanced stereo rendering.", delegate(UnityEngine.Object obj, bool last, int selected) 
			{
				PlayerSettings.singlePassStereoRendering = true;
			}, null, "Enable Single-Pass");
		}
#endif

		if (LightmapSettings.lightmaps.Length > 0 && LightmapSettings.lightmapsMode != LightmapsMode.NonDirectional)
		{
		    AddFix ("Optimize Lightmap Directionality", "For GPU performance, please don't use directional lightmaps.", delegate(UnityEngine.Object obj, bool last, int selected) 
		    {
				LightmapSettings.lightmapsMode = LightmapsMode.NonDirectional;
			}, null, "Switch Lightmap Mode");
		}

#if UNITY_5_4_OR_NEWER
		if (Lightmapping.realtimeGI)
		{
			AddFix ("Optimize Realtime GI", "For GPU performance, please don't use real-time global illumination. (Set Lightmapping.realtimeGI = false.)", delegate(UnityEngine.Object obj, bool last, int selected) 
			{
				Lightmapping.realtimeGI = false;
			}, null, "Disable Realtime GI");
		}
#endif

		var lights = GameObject.FindObjectsOfType<Light> ();
		for (int i = 0; i < lights.Length; ++i) 
		{
#if UNITY_5_4_OR_NEWER
			if (lights [i].type != LightType.Directional && !lights [i].isBaked && IsLightBaked(lights[i]))
			{
				AddFix ("Optimize Light Baking", "For GPU performance, please bake lightmaps to avoid realtime lighting cost.", delegate(UnityEngine.Object obj, bool last, int selected) 
				{
					if (last)
					{
						Lightmapping.Bake ();
					}
				}, lights[i], "Bake Lightmaps");
			}
#endif

			if (lights [i].shadows != LightShadows.None && !IsLightBaked(lights[i]))
			{
			    AddFix ("Optimize Shadows", "For CPU performance, please disable shadows on realtime lights.", delegate(UnityEngine.Object obj, bool last, int selected) 
			    {
			    	Light thisLight = (Light)obj;
					thisLight.shadows = LightShadows.None;
				}, lights [i], "Disable Shadows");
			}
		}

/*
		// CP: I think this should modify the max number of simultaneous voices in the audio settings rather than
		// the number of sources.  Sources don't cost anything if they aren't playing simultaneously.
		// Couldn't figure out if there's an API for max voices.
		var sources = GameObject.FindObjectsOfType<AudioSource> ();
		if (sources.Length > 16 &&
		    EditorUtility.DisplayDialog ("Optimize Audio Source Count", "For CPU performance, please disable all but the top 16 AudioSources.", "Use recommended", "Skip")) {
			Array.Sort(sources, (a, b) => { return a.priority.CompareTo(b.priority); });
			for (int i = 16; i < sources.Length; ++i) {
				sources[i].enabled = false;
			}
		}
*/

		var clips = GameObject.FindObjectsOfType<AudioClip> ();
		for (int i = 0; i < clips.Length; ++i) 
		{
			if (clips [i].loadType == AudioClipLoadType.DecompressOnLoad)
			{
				AddFix("Audio Loading", "For fast loading, please don't use decompress on load for audio clips", delegate(UnityEngine.Object obj, bool last, int selected)
				{
					AudioClip thisClip = (AudioClip)obj;
					if (selected == 0)
					{
						SetAudioLoadType(thisClip, AudioClipLoadType.CompressedInMemory, last);
					}
					else
					{
						SetAudioLoadType(thisClip, AudioClipLoadType.Streaming, last);
					}
					
				}, clips [i], "Change to Compressed in Memory", "Change to Streaming");
			}

			if (clips [i].preloadAudioData)
			{
				AddFix("Audio Preload", "For fast loading, please don't preload data for audio clips.", delegate(UnityEngine.Object obj, bool last, int selected)
				{
					SetAudioPreload(clips[i], false, last);
				}, clips [i], "Fix");
			}
		}

		if (Physics.defaultContactOffset < 0.01f)
		{
		    AddFix ("Optimize Contact Offset", "For CPU performance, please don't use default contact offset below 0.01.", delegate(UnityEngine.Object obj, bool last, int selected)
			{
				Physics.defaultContactOffset = 0.01f;
			}, null, "Fix");
		}
		
		if (Physics.sleepThreshold < 0.005f)
		{
			AddFix ("Optimize Sleep Threshold", "For CPU performance, please don't use sleep threshold below 0.005.", delegate(UnityEngine.Object obj, bool last, int selected)
			{
				Physics.sleepThreshold = 0.005f;
			}, null, "Fix");
		}

#if UNITY_5_4_OR_NEWER
		if (Physics.defaultSolverIterations > 8)
		{
		    AddFix ("Optimize Solver Iterations", "For CPU performance, please don't use excessive solver iteration counts.", delegate(UnityEngine.Object obj, bool last, int selected)
			{
				Physics.defaultSolverIterations = 8;
			}, null, "Fix");
		}
#endif

		var colliders = GameObject.FindObjectsOfType<Collider> ();
		for (int i = 0; i < colliders.Length; ++i) 
		{
			
			// CP: unsure when attachedRigidbody is init'd, so search parents to be sure.
			if (!colliders [i].gameObject.isStatic && colliders [i].attachedRigidbody == null && colliders[i].GetComponent<Rigidbody>() == null && FindComponentInParents<Rigidbody>(colliders[i].gameObject) == null)
			{
			    AddFix ("Optimize Nonstatic Collider", "For CPU performance, please make static or attach a Rigidbody to non-static colliders.", delegate(UnityEngine.Object obj, bool last, int selected)
			    {
			    	Collider thisCollider = (Collider)obj;
			    	if (selected == 0)
			    	{
			    		thisCollider.gameObject.isStatic = true;
			    	}
			    	else
			    	{
						var rb = thisCollider.gameObject.AddComponent<Rigidbody> ();
						rb.isKinematic = true;
					}
				}, colliders[i], "Make Static", "Add Rigidbody");
			}
		}

		var materials = Resources.FindObjectsOfTypeAll<Material> ();
		for (int i = 0; i < materials.Length; ++i) 
		{
			if (materials [i].shader.name.Contains ("Parallax") || materials [i].IsKeywordEnabled ("_PARALLAXMAP"))
			{
			    AddFix ("Optimize Shading", "For GPU performance, please don't use parallax-mapped materials.", delegate(UnityEngine.Object obj, bool last, int selected)
			    {
			    	Material thisMaterial = (Material)obj;
					if (thisMaterial.IsKeywordEnabled ("_PARALLAXMAP"))
					{
						thisMaterial.DisableKeyword ("_PARALLAXMAP");
					}

					if (thisMaterial.shader.name.Contains ("Parallax")) 
					{
						var newName = thisMaterial.shader.name.Replace ("-ParallaxSpec", "-BumpSpec");
						newName = newName.Replace ("-Parallax", "-Bump");
						var newShader = Shader.Find (newName);
						if (newShader)
						{
							thisMaterial.shader = newShader;
						}
						else
						{
							Debug.LogWarning ("Unable to find a replacement for shader " + materials [i].shader.name);
						}
					}
				}, materials[i], "Fix");
			}
		}

		var renderers = GameObject.FindObjectsOfType<Renderer> ();
		for (int i = 0; i < renderers.Length; ++i) 
		{
			if (renderers [i].sharedMaterial == null)
			{
				AddFix("Instanced Materials", "Please avoid instanced materials on renderers.", null, renderers [i]);
			}
		}
		
		var overlays = GameObject.FindObjectsOfType<OVROverlay> ();
		if (overlays.Length > 4)
		{
		    AddFix ("Optimize VR Layer Count", "For GPU performance, please use 4 or fewer VR layers.", delegate(UnityEngine.Object obj, bool last, int selected)
		    {
				for (int i = 4; i < OVROverlay.instances.Length; ++i)
				{
					OVROverlay.instances[i].enabled = false;
				}
			}, null, "Fix");
		}
	}

	static void CheckRuntimeCommonIssues()
	{
		if (!OVRPlugin.occlusionMesh)
		{
			AddFix ("Optimize Occlusion Mesh", "For GPU performance, please use occlusion mesh.", delegate(UnityEngine.Object obj, bool last, int selected)
			{
				OVRPlugin.occlusionMesh = true;
			}, null, "Fix");
		}
		
		if (OVRManager.instance != null && !OVRManager.instance.useRecommendedMSAALevel)
		{
			AddFix("Optimize MSAA", "OVRManager can select the optimal antialiasing for the installed hardware at runtime. Recommend enabling this.", delegate(UnityEngine.Object obj, bool last, int selected)
			{
                OVRManager.instance.useRecommendedMSAALevel = true;
			}, null, "Fix");
		}

		if (UnityEngine.XR.XRSettings.eyeTextureResolutionScale > 1.5)
		{
			AddFix ("Optimize Render Scale", "For CPU performance, please don't use render scale over 1.5.", delegate(UnityEngine.Object obj, bool last, int selected)
			{
				UnityEngine.XR.XRSettings.eyeTextureResolutionScale = 1.5f;
			}, null, "Fix");
		}
	}

	static void CheckStaticAndroidIssues ()
	{
		AndroidSdkVersions recommendedAndroidSdkVersion = AndroidSdkVersions.AndroidApiLevel19;
		if ((int)PlayerSettings.Android.minSdkVersion < (int)recommendedAndroidSdkVersion)
		{
			AddFix ("Optimize Android API Level", "To avoid legacy work-arounds, please require at least API level " + (int)recommendedAndroidSdkVersion, delegate(UnityEngine.Object obj, bool last, int selected)
			{
				PlayerSettings.Android.minSdkVersion = recommendedAndroidSdkVersion;
			}, null, "Fix");
		}

		if (RenderSettings.skybox)
		{
			AddFix ("Optimize Clearing", "For GPU performance, please don't use Unity's built-in Skybox.", delegate(UnityEngine.Object obj, bool last, int selected) 
			{
				RenderSettings.skybox = null;
			}, null, "Clear Skybox");
		}
		
		var materials = Resources.FindObjectsOfTypeAll<Material> ();
		for (int i = 0; i < materials.Length; ++i) 
		{
			if (materials [i].IsKeywordEnabled ("_SPECGLOSSMAP") || materials [i].IsKeywordEnabled ("_METALLICGLOSSMAP"))
			{
			    AddFix ("Optimize Specular Material", "For GPU performance, please don't use specular shader on materials.", delegate(UnityEngine.Object obj, bool last, int selected)
			    {
			    	Material thisMaterial = (Material)obj;
					thisMaterial.DisableKeyword ("_SPECGLOSSMAP");
					thisMaterial.DisableKeyword ("_METALLICGLOSSMAP");
				}, materials[i], "Fix");
			}

			if (materials [i].passCount > 1)
			{
				AddFix ("Material Passes", "Please use 2 or fewer passes in materials.", null, materials[i]);
			}
		}

#if UNITY_5_5_OR_NEWER
		ScriptingImplementation backend = PlayerSettings.GetScriptingBackend(UnityEditor.BuildTargetGroup.Android);
		if (backend != UnityEditor.ScriptingImplementation.IL2CPP)
		{
			AddFix ("Optimize Scripting Backend", "For CPU performance, please use IL2CPP.", delegate(UnityEngine.Object obj, bool last, int selected)
			{
				PlayerSettings.SetScriptingBackend(UnityEditor.BuildTargetGroup.Android, UnityEditor.ScriptingImplementation.IL2CPP);
			}, null, "Fix");
		}
#else
		ScriptingImplementation backend = (ScriptingImplementation)PlayerSettings.GetPropertyInt("ScriptingBackend", UnityEditor.BuildTargetGroup.Android);
		if (backend != UnityEditor.ScriptingImplementation.IL2CPP)
		{
			AddFix ("Optimize Scripting Backend", "For CPU performance, please use IL2CPP.", delegate(UnityEngine.Object obj, bool last, int selected)
			{
				PlayerSettings.SetPropertyInt("ScriptingBackend", (int)UnityEditor.ScriptingImplementation.IL2CPP, UnityEditor.BuildTargetGroup.Android);
			}, null, "Fix");
		}
#endif

		var monoBehaviours = GameObject.FindObjectsOfType<MonoBehaviour> ();
		System.Type effectBaseType = System.Type.GetType ("UnityStandardAssets.ImageEffects.PostEffectsBase");
		if (effectBaseType != null) 
		{
			for (int i = 0; i < monoBehaviours.Length; ++i) 
			{
				if (monoBehaviours [i].GetType ().IsSubclassOf (effectBaseType))
				{
					AddFix ("Image Effects", "Please don't use image effects.", null, monoBehaviours[i]);
				}
			}
		}

		var textures = Resources.FindObjectsOfTypeAll<Texture2D> ();

		int maxTextureSize = 1024 * (1 << QualitySettings.masterTextureLimit);
		maxTextureSize = maxTextureSize * maxTextureSize;

		for (int i = 0; i < textures.Length; ++i) 
		{
			if (textures [i].filterMode == FilterMode.Trilinear && textures [i].mipmapCount == 1)
			{
			    AddFix ("Optimize Texture Filtering", "For GPU performance, please generate mipmaps or disable trilinear filtering for textures.", delegate(UnityEngine.Object obj, bool last, int selected)
			    {
			    	Texture2D thisTexture = (Texture2D)obj;
			    	if (selected == 0)
			    	{
						thisTexture.filterMode = FilterMode.Bilinear;
					}
					else
					{
						SetTextureUseMips(thisTexture, true, last);
					}
				}, textures[i], "Switch to Bilinear", "Generate Mipmaps");
			}
		}

		var projectors = GameObject.FindObjectsOfType<Projector> ();
		if (projectors.Length > 0)
		{
		    AddFix ("Optimize Projectors", "For GPU performance, please don't use projectors.", delegate(UnityEngine.Object obj, bool last, int selected)
		    {
				Projector[] thisProjectors = GameObject.FindObjectsOfType<Projector> ();
				for (int i = 0; i < thisProjectors.Length; ++i)
				{
					thisProjectors[i].enabled = false;
				}
			}, null, "Disable Projectors");
		}

		if (EditorUserBuildSettings.androidBuildSubtarget != MobileTextureSubtarget.ASTC)
		{
		    AddFix ("Optimize Texture Compression", "For GPU performance, please use ASTC.", delegate(UnityEngine.Object obj, bool last, int selected)
		    {
				EditorUserBuildSettings.androidBuildSubtarget = MobileTextureSubtarget.ASTC;
			}, null, "Fix");
		}

		var cameras = GameObject.FindObjectsOfType<Camera> ();
		int clearCount = 0;
		for (int i = 0; i < cameras.Length; ++i) 
		{
			if (cameras [i].clearFlags != CameraClearFlags.Nothing && cameras [i].clearFlags != CameraClearFlags.Depth)
				++clearCount;
		}

		if (clearCount > 2)
		{
			AddFix ("Camera Clears", "Please use 2 or fewer clears.", null, null);
		}
	}

	static void CheckRuntimeAndroidIssues()
	{
		if (UnityStats.usedTextureMemorySize + UnityStats.vboTotalBytes > 1000000)
		{
			AddFix ("Graphics Memory", "Please use less than 1GB of vertex and texture memory.", null, null);
		}
		
		if (OVRManager.cpuLevel < 0 || OVRManager.cpuLevel > 3)
		{
		    AddFix ("Optimize CPU level", "For battery life, please use a safe CPU level.", delegate(UnityEngine.Object obj, bool last, int selected)
		    {
				OVRManager.cpuLevel = 2;
			}, null, "Set to CPU2");
		}

		if (OVRManager.gpuLevel < 0 || OVRManager.gpuLevel > 3)
		{
			AddFix ("Optimize GPU level", "For battery life, please use a safe GPU level.", delegate(UnityEngine.Object obj, bool last, int selected)
			{
				OVRManager.gpuLevel = 2;
			}, null, "Set to GPU2");
		}

		if (UnityStats.triangles > 100000 || UnityStats.vertices > 100000)
		{
			AddFix ("Triangles and Verts", "Please use less than 100000 triangles or vertices.", null, null);
		}

		// Warn for 50 if in non-VR mode?
		if (UnityStats.drawCalls > 100)
		{
			AddFix ("Draw Calls", "Please use less than 100 draw calls.", null, null);
		}
	}
	
	
	enum LightmapType {Realtime = 4, Baked = 2, Mixed = 1};
	
	static bool IsLightBaked(Light light)
    {
    	#if UNITY_5_6_OR_NEWER
    		return light.lightmapBakeType == LightmapBakeType.Baked; 
    	#elif UNITY_5_5_OR_NEWER
    		return light.lightmappingMode == LightmappingMode.Baked; 
    	#else
			SerializedObject serialObj = new SerializedObject(light); 
			SerializedProperty lightmapProp = serialObj.FindProperty("m_Lightmapping");
			return (LightmapType)lightmapProp.intValue == LightmapType.Baked;
    	#endif
    }
    
    static void SetAudioPreload( AudioClip clip, bool preload, bool refreshImmediately)
	{
		if ( clip != null ) 
		{
			string assetPath = AssetDatabase.GetAssetPath( clip );
			AudioImporter importer = AssetImporter.GetAtPath( assetPath ) as AudioImporter;
			if (importer != null)
			{
				if (preload != importer.preloadAudioData)
				{
					importer.preloadAudioData = preload;

					AssetDatabase.ImportAsset( assetPath );
					if (refreshImmediately)
					{
						AssetDatabase.Refresh();
					}
				}
			}
		}
	}
	
	static void SetAudioLoadType( AudioClip clip, AudioClipLoadType loadType, bool refreshImmediately)
	{
		if ( clip != null ) 
		{
			string assetPath = AssetDatabase.GetAssetPath( clip );
			AudioImporter importer = AssetImporter.GetAtPath( assetPath ) as AudioImporter;
			if (importer != null)
			{
				if (loadType != importer.defaultSampleSettings.loadType)
				{
					AudioImporterSampleSettings settings = importer.defaultSampleSettings;
					settings.loadType = loadType;
					importer.defaultSampleSettings = settings;

					AssetDatabase.ImportAsset( assetPath );
					if (refreshImmediately)
					{
						AssetDatabase.Refresh();
					}
				}
			}
		}
	}
	
	public static void SetTextureUseMips( Texture texture, bool useMips, bool refreshImmediately)
	{
		if ( texture != null ) 
		{
			string assetPath = AssetDatabase.GetAssetPath( texture );
			TextureImporter tImporter = AssetImporter.GetAtPath( assetPath ) as TextureImporter;
			if ( tImporter != null && tImporter.mipmapEnabled != useMips)
			{
				tImporter.mipmapEnabled = useMips;

				AssetDatabase.ImportAsset( assetPath );
				if (refreshImmediately)
				{
					AssetDatabase.Refresh();
				}
			}
		}
	}
	
	static T FindComponentInParents<T>(GameObject obj) where T : Component
	{
		T component = null;
		if (obj != null)
		{
			Transform parent = obj.transform.parent;
			if (parent != null)
			{
				do
				{
					component = parent.GetComponent(typeof(T)) as T;
					parent = parent.parent;
				} while (parent != null && component == null);
			}
		}
		return component;
	}
}

#endif
