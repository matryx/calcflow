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

#if UNITY_5_4_OR_NEWER
using UnityEngine;
using UnityEditor;
using UnityEditor.Callbacks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.IO;
using System.Diagnostics;

[InitializeOnLoad]
class OVRPluginUpdater
{
	class PluginPackage
	{
		public string RootPath;
		public System.Version Version;
		public Dictionary<BuildTarget, string> Plugins = new Dictionary<BuildTarget, string>();

		public bool IsBundledPluginPackage()
		{
			return (RootPath == GetBundledPluginRootPath());
		}

		public bool IsEnabled()
		{
			// TODO: Check each individual platform rather than using the Win64 DLL status for the overall package status.
			string win64PluginPath = "";
			if (Plugins.TryGetValue(BuildTarget.StandaloneWindows64, out win64PluginPath))
			{
				return File.Exists(win64PluginPath);
			}

			return false;
		}
	}

    static OVRPluginUpdater()
	{
		if (ShouldAttemptPluginUpdate())
		{
			AttemptPluginUpdate(true);
		}
	}

	private static PluginPackage GetPluginPackage(string rootPath)
	{
		return new PluginPackage()
		{
			RootPath = rootPath,
			Version = GetPluginVersion(rootPath),
			Plugins = new Dictionary<BuildTarget, string>()
			{
				{ BuildTarget.Android, rootPath + GetPluginBuildTargetSubPath(BuildTarget.Android) },
				{ BuildTarget.StandaloneOSXUniversal, rootPath + GetPluginBuildTargetSubPath(BuildTarget.StandaloneOSXUniversal) },
				{ BuildTarget.StandaloneWindows, rootPath + GetPluginBuildTargetSubPath(BuildTarget.StandaloneWindows) },
				{ BuildTarget.StandaloneWindows64, rootPath + GetPluginBuildTargetSubPath(BuildTarget.StandaloneWindows64) },
			}
		};
	}

	private static PluginPackage GetBundledPluginPackage()
	{
		return GetPluginPackage(GetBundledPluginRootPath());
	}

	private static List<PluginPackage> GetAllUtilitiesPluginPackages()
	{
		string pluginRootPath = GetUtilitiesPluginRootPath();
		List<PluginPackage> packages = new List<PluginPackage>();

		if (Directory.Exists(pluginRootPath))
		{
			var dirs = Directory.GetDirectories(pluginRootPath);

			foreach(string dir in dirs)
			{
				packages.Add(GetPluginPackage(dir));
			}
		}

		return packages;
	}

	private static string GetCurrentProjectPath()
	{
		return Directory.GetParent(Application.dataPath).FullName;
	}

	private static string GetUtilitiesPluginRootPath()
	{
		return GetUtilitiesRootPath() + @"/Plugins";
	}

	private static string GetUtilitiesRootPath()
	{
		var so = ScriptableObject.CreateInstance(typeof(OVRPluginUpdaterStub));
		var script = MonoScript.FromScriptableObject(so);
		string assetPath = AssetDatabase.GetAssetPath(script);
		string editorDir = Directory.GetParent(assetPath).FullName;
		string ovrDir = Directory.GetParent(editorDir).FullName;

		return ovrDir;
	}

	private static string GetBundledPluginRootPath()
	{
		string basePath = EditorApplication.applicationContentsPath;
#if UNITY_5_4
		string pluginPath = @"/VR/Unity";
#elif UNITY_5_5_OR_NEWER
		string pluginPath = @"/UnityExtensions/Unity/VR";
#endif

		return basePath + pluginPath;
	}

	private static string GetPluginBuildTargetSubPath(BuildTarget target)
	{
		string path = string.Empty;

		switch (target)
		{
			case BuildTarget.Android:
				path = @"/Android/OVRPlugin.aar";
				break;
			case BuildTarget.StandaloneOSXUniversal:
				path = @"/OSXUniversal/OVRPlugin.bundle";
				break;
			case BuildTarget.StandaloneWindows:
				path = @"/Win/OVRPlugin.dll";
				break;
			case BuildTarget.StandaloneWindows64:
				path = @"/Win64/OVRPlugin.dll";
				break;
			default:
				throw new ArgumentException("Attempted GetPluginBuildTargetSubPath() for unsupported BuildTarget: " + target);
		}

		return path;
	}

	private static string GetDisabledPluginSuffix()
	{
		return @".disabled";
	}

	private static System.Version GetPluginVersion(string path)
	{
		System.Version invalidVersion = new System.Version("0.0.0");
		System.Version pluginVersion = invalidVersion;

		try
		{
			pluginVersion = new System.Version(Path.GetFileName(path));
		}
		catch
		{
			pluginVersion = invalidVersion;
		}

		if (pluginVersion == invalidVersion)
		{
			//Unable to determine version from path, fallback to Win64 DLL meta data
			path += GetPluginBuildTargetSubPath(BuildTarget.StandaloneWindows64);
			if (!File.Exists(path))
			{
				path += GetDisabledPluginSuffix();
				if (!File.Exists(path))
				{
					return invalidVersion;
				}
			}

			FileVersionInfo pluginVersionInfo = FileVersionInfo.GetVersionInfo(path);
			if (pluginVersionInfo == null || pluginVersionInfo.ProductVersion == null || pluginVersionInfo.ProductVersion == "")
			{
				return invalidVersion;
			}

			pluginVersion = new System.Version(pluginVersionInfo.ProductVersion);
		}

		return pluginVersion;
	}
	
	private static bool ShouldAttemptPluginUpdate()
	{
		return autoUpdateEnabled && !restartPending && !Application.isPlaying;
	}

	private static void DisableAllUtilitiesPluginPackages()
	{
		List<PluginPackage> allUtilsPluginPkgs = GetAllUtilitiesPluginPackages();

		foreach(PluginPackage pluginPkg in allUtilsPluginPkgs)
		{
			foreach(string path in pluginPkg.Plugins.Values)
			{
				if ((Directory.Exists(path)) || (File.Exists(path)))
				{
					string basePath = GetCurrentProjectPath();
					string relPath = path.Substring(basePath.Length + 1);

					AssetDatabase.MoveAsset(relPath, relPath + GetDisabledPluginSuffix());
					AssetDatabase.ImportAsset(relPath + GetDisabledPluginSuffix(), ImportAssetOptions.ForceUpdate);
				}
			}
		}

		AssetDatabase.Refresh();
		AssetDatabase.SaveAssets();
	}

	private static void EnablePluginPackage(PluginPackage pluginPkg)
	{
		foreach(var kvp in pluginPkg.Plugins)
		{
			BuildTarget platform = kvp.Key;
			string path = kvp.Value;

			if ((Directory.Exists(path + GetDisabledPluginSuffix())) || (File.Exists(path + GetDisabledPluginSuffix())))
			{
				string basePath = GetCurrentProjectPath();
				string relPath = path.Substring(basePath.Length + 1);

				AssetDatabase.MoveAsset(relPath + GetDisabledPluginSuffix(), relPath);
				AssetDatabase.ImportAsset(relPath, ImportAssetOptions.ForceUpdate);

				PluginImporter pi = PluginImporter.GetAtPath(relPath) as PluginImporter;
				if (pi == null)
				{
					continue;
				}

				pi.SetCompatibleWithAnyPlatform(false);

				switch (platform)
				{
					case BuildTarget.Android:
						pi.SetCompatibleWithPlatform(BuildTarget.Android, true);
						pi.SetPlatformData(BuildTarget.Android, "CPU", "ARMv7");
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneOSXUniversal, false);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneOSXIntel, false);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneOSXIntel64, false);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneWindows, false);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneWindows64, false);
						pi.SetCompatibleWithEditor(false);
						break;
					case BuildTarget.StandaloneOSXUniversal:
						pi.SetCompatibleWithPlatform(BuildTarget.Android, false);
						pi.SetPlatformData(BuildTarget.Android, "CPU", "ARMv7");
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneOSXUniversal, true);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneOSXIntel, true);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneOSXIntel64, true);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneWindows, false);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneWindows64, false);
						pi.SetCompatibleWithEditor(true);
						pi.SetEditorData("CPU", "AnyCPU");
						pi.SetEditorData("OS", "OSX");
						pi.SetPlatformData("Editor", "CPU", "AnyCPU");
						pi.SetPlatformData("Editor", "OS", "OSX");
						break;
					case BuildTarget.StandaloneWindows:
						pi.SetCompatibleWithPlatform(BuildTarget.Android, false);
						pi.SetPlatformData(BuildTarget.Android, "CPU", "ARMv7");
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneOSXUniversal, false);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneOSXIntel, false);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneOSXIntel64, false);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneWindows, true);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneWindows64, false);
						pi.SetCompatibleWithEditor(true);
						pi.SetEditorData("CPU", "X86");
						pi.SetEditorData("OS", "Windows");
						pi.SetPlatformData("Editor", "CPU", "X86");
						pi.SetPlatformData("Editor", "OS", "Windows");
						break;
					case BuildTarget.StandaloneWindows64:
						pi.SetCompatibleWithPlatform(BuildTarget.Android, false);
						pi.SetPlatformData(BuildTarget.Android, "CPU", "ARMv7");
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneOSXUniversal, false);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneOSXIntel, false);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneOSXIntel64, false);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneWindows, false);
						pi.SetCompatibleWithPlatform(BuildTarget.StandaloneWindows64, true);
						pi.SetCompatibleWithEditor(true);
						pi.SetEditorData("CPU", "X86_64");
						pi.SetEditorData("OS", "Windows");
						pi.SetPlatformData("Editor", "CPU", "X86_64");
						pi.SetPlatformData("Editor", "OS", "Windows");
						break;
					default:
						throw new ArgumentException("Attempted EnablePluginPackage() for unsupported BuildTarget: " + platform);
				}

				AssetDatabase.ImportAsset(relPath, ImportAssetOptions.ForceUpdate);
			}
		}

		AssetDatabase.Refresh();
		AssetDatabase.SaveAssets();
	}

	private static bool restartPending = false;
	private static readonly string autoUpdateEnabledKey = "Oculus_Utilities_OVRPluginUpdater_AutoUpdate_" + OVRManager.utilitiesVersion;
	private static bool autoUpdateEnabled
	{
		get {
			return PlayerPrefs.GetInt(autoUpdateEnabledKey, 1) == 1;
		}

		set {
			PlayerPrefs.SetInt(autoUpdateEnabledKey, value ? 1 : 0);
		}
	}

	[MenuItem("Tools/Oculus/Disable OVR Utilities Plugin")]
	private static void AttemptPluginDisable()
	{
		PluginPackage bundledPluginPkg = GetBundledPluginPackage();
		List<PluginPackage> allUtilsPluginPkgs = GetAllUtilitiesPluginPackages();

		PluginPackage enabledUtilsPluginPkg = null;

		foreach(PluginPackage pluginPkg in allUtilsPluginPkgs)
		{
			if (pluginPkg.IsEnabled())
			{
				if ((enabledUtilsPluginPkg == null) || (pluginPkg.Version > enabledUtilsPluginPkg.Version))
				{
					enabledUtilsPluginPkg = pluginPkg;
				}
			}
		}

		if (enabledUtilsPluginPkg == null)
		{
			if (EditorUtility.DisplayDialog("Disable Oculus Utilities Plugin", "The OVRPlugin included with Oculus Utilities is already disabled. The OVRPlugin bundled with the Unity Editor will continue to be used.\n\nBundled version: " + bundledPluginPkg.Version, "Ok", ""))
			{
				return;
			}
		}
		else
		{
			if (EditorUtility.DisplayDialog("Disable Oculus Utilities Plugin", "Do you want to disable the OVRPlugin included with Oculus Utilities and revert to the OVRPlugin bundled with the Unity Editor?\n\nCurrent version: " + enabledUtilsPluginPkg.Version + "\nBundled version: " + bundledPluginPkg.Version, "Yes", "No"))
			{
				DisableAllUtilitiesPluginPackages();

				if (EditorUtility.DisplayDialog("Restart Unity", "OVRPlugin has been updated to " + bundledPluginPkg.Version + ".\n\nPlease restart the Unity Editor to complete the update process. You may need to manually relaunch Unity if you are using Unity 5.6 and higher.", "Restart", "Not Now"))
				{
					RestartUnityEditor();
				}
			}
		}

	}

	[MenuItem("Tools/Oculus/Update OVR Utilities Plugin")]
	private static void RunPluginUpdate()
	{
		AttemptPluginUpdate(false);
	}

	private static void AttemptPluginUpdate(bool triggeredByAutoUpdate)
	{
		autoUpdateEnabled = true;

		PluginPackage bundledPluginPkg = GetBundledPluginPackage();
		List<PluginPackage> allUtilsPluginPkgs = GetAllUtilitiesPluginPackages();

		PluginPackage enabledUtilsPluginPkg = null;
		PluginPackage newestUtilsPluginPkg = null;

		foreach(PluginPackage pluginPkg in allUtilsPluginPkgs)
		{
			if ((newestUtilsPluginPkg == null) || (pluginPkg.Version > newestUtilsPluginPkg.Version))
			{
				newestUtilsPluginPkg = pluginPkg;
			}

			if (pluginPkg.IsEnabled())
			{
				if ((enabledUtilsPluginPkg == null) || (pluginPkg.Version > enabledUtilsPluginPkg.Version))
				{
					enabledUtilsPluginPkg = pluginPkg;
				}
			}
		}

		PluginPackage targetPluginPkg = null;

		if ((newestUtilsPluginPkg != null) && (newestUtilsPluginPkg.Version > bundledPluginPkg.Version))
		{
			if ((enabledUtilsPluginPkg == null) || (enabledUtilsPluginPkg.Version != newestUtilsPluginPkg.Version))
			{
				targetPluginPkg = newestUtilsPluginPkg;
			}
		}
		else if ((enabledUtilsPluginPkg != null) && (enabledUtilsPluginPkg.Version < bundledPluginPkg.Version))
		{
			targetPluginPkg = bundledPluginPkg;
		}
         
		System.Version currentVersion = (enabledUtilsPluginPkg != null) ? enabledUtilsPluginPkg.Version : bundledPluginPkg.Version;

		if (targetPluginPkg == null)
		{
			if (!triggeredByAutoUpdate)
			{
				EditorUtility.DisplayDialog("Update Oculus Utilities Plugin", "OVRPlugin is already up to date.\n\nCurrent version: " + currentVersion + "\nBundled version: " + bundledPluginPkg.Version, "Ok", "");
			}

			return; // No update necessary.
		}

		System.Version targetVersion = targetPluginPkg.Version;

		int dialogResult = EditorUtility.DisplayDialogComplex("Update Oculus Utilities Plugin", "Oculus Utilities has detected that a newer OVRPlugin is available. Using the newest version is recommended. Do you want to enable it?\n\nCurrent version: " + currentVersion + "\nAvailable version: " + targetVersion, "Yes", "No, Don't Ask Again", "No");

		bool userAcceptsUpdate = false;

		switch (dialogResult)
		{
			case 0: // "Yes"
				userAcceptsUpdate = true;
				break;
			case 1: // "No, Don't Ask Again"
				autoUpdateEnabled = false;

				EditorUtility.DisplayDialog("Oculus Utilities OVRPlugin", "To manually update in the future, use the following menu option:\n\n[Tools -> Oculus -> Update OVR Utilities Plugin]", "Ok", "");
				return;
			case 2: // "No"
				return;
		}

		if (userAcceptsUpdate)
		{
			DisableAllUtilitiesPluginPackages();

			if (!targetPluginPkg.IsBundledPluginPackage())
			{
				EnablePluginPackage(targetPluginPkg);
			}

			if (EditorUtility.DisplayDialog("Restart Unity", "OVRPlugin has been updated to " + targetPluginPkg.Version + ".\n\nPlease restart the Unity Editor to complete the update process. You may need to manually relaunch Unity if you are using Unity 5.6 and higher.", "Restart", "Not Now"))
			{
				RestartUnityEditor();
			}
		}
	}

	private static void RestartUnityEditor()
	{
		restartPending = true;
		EditorApplication.OpenProject(GetCurrentProjectPath());
	}
}
#endif

