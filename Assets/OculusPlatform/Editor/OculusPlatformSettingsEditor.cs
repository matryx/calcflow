namespace Oculus.Platform
{
  using System;
  using System.Collections;
  using System.Collections.Generic;
  using System.IO;
  using System.Reflection;
  using UnityEditor;
  using UnityEngine;

  [CustomEditor(typeof(PlatformSettings))]
  public class OculusPlatformSettingsEditor : Editor
  {
#if UNITY_EDITOR_WIN
    static bool windowsPlatformAllowedInEditor = true;
    GUIContent riftAppIDLabel = new GUIContent("Oculus Rift App Id [?]",
      "This AppID will be used when building to the Windows Standalone target.");
#else
    static bool windowsPlatformAllowedInEditor = false;
    GUIContent riftAppIDLabel = new GUIContent("Oculus Rift App Id [?]",
      "This AppID will be used when building to the Windows Standalone target. It will also be used when running in the editor so long as \"Use Oculus Platform\" is checked in editor settings below");
#endif
    GUIContent gearAppIDLabel = new GUIContent("Gear VR App Id [?]", "This AppID will be used when building to the Android target");
    GUIContent userTokenLabel = new GUIContent("Oculus User Token [?]",
      "This token will be used when running in the editor if not connected to the Oculus Platform");
    GUIContent useStandalonePlatformLabel = new GUIContent("Use User Token [?]",
      "If this is checked your app will use a debug platform with the User Token below.  Otherwise your app will connect to the Oculus Platform.  This setting only applies to the Unity Editor");

    private bool showUnityEditorSettings = false; //always expand this if we need an hardcoded token
    private bool showBuildSettings = false;

    void OnEnable()
    {
      // Always expand the editor settings if we're not on windows
      showUnityEditorSettings = !windowsPlatformAllowedInEditor;

      // If we don't have a rift app id assigned, make sure the editor settings are expanded
      if (windowsPlatformAllowedInEditor && String.IsNullOrEmpty(PlatformSettings.AppID))
      {
        showUnityEditorSettings = true;
      }
    }

    string MakeTextBox(GUIContent label, string variable) {
      return GUIHelper.MakeControlWithLabel(label, () => {
        GUI.changed = false;
        var result = EditorGUILayout.TextField(variable);
        SetDirtyOnGUIChange();
        return result;
      });
    }

    bool MakeToggle(GUIContent label, bool variable) {
      return GUIHelper.MakeControlWithLabel(label, () => {
        GUI.changed = false;
        var result = EditorGUILayout.Toggle(variable);
        SetDirtyOnGUIChange();
        return result;
      });
    }

    [UnityEditor.MenuItem("Oculus Platform/Edit Settings")]
    public static void Edit()
    {
      var settings = PlatformSettings.Instance;

      UnityEditor.Selection.activeObject = settings;

      if (String.IsNullOrEmpty(PlatformSettings.MobileAppID))
      {
        PlatformSettings.MobileAppID = PlatformSettings.AppID;
      }
    }

    public override void OnInspectorGUI()
    {
      bool usingWindowsPlatformInEditor = !PlatformSettings.UseStandalonePlatform && windowsPlatformAllowedInEditor;

      PlatformSettings.AppID = MakeTextBox(riftAppIDLabel, PlatformSettings.AppID);
      PlatformSettings.MobileAppID = MakeTextBox(gearAppIDLabel, PlatformSettings.MobileAppID);

      if (GUILayout.Button("Create / Find your app on https://dashboard.oculus.com"))
      {
        UnityEngine.Application.OpenURL("https://dashboard.oculus.com/");
      }
      EditorGUILayout.Separator();

      MakeEditorSettings(windowsPlatformAllowedInEditor, usingWindowsPlatformInEditor);

      showBuildSettings = EditorGUILayout.Foldout(showBuildSettings, "Build Settings");
      if (showBuildSettings)
      {
        GUIHelper.HInset(6, () => {
          if (!PlayerSettings.virtualRealitySupported)
          {
            EditorGUILayout.HelpBox("VR Support isn't enabled in the Player Settings", MessageType.Warning);
          }
          else
          {
            EditorGUILayout.HelpBox("VR Support is enabled", MessageType.Info);
          }

          PlayerSettings.virtualRealitySupported = MakeToggle(new GUIContent("Virtual Reality Support"), PlayerSettings.virtualRealitySupported);
          PlayerSettings.bundleVersion = MakeTextBox(new GUIContent("Bundle Version"), PlayerSettings.bundleVersion);
#if UNITY_5_3 || UNITY_5_4 || UNITY_5_5
          PlayerSettings.bundleIdentifier = MakeTextBox(new GUIContent("Bundle Identifier"), PlayerSettings.bundleIdentifier);
#else
          PlayerSettings.applicationIdentifier = MakeTextBox(new GUIContent("Bundle Identifier"), PlayerSettings.applicationIdentifier);
#endif
        });
      }

      EditorGUILayout.Separator();
    }

    void MakeEditorSettings(bool windowsPlatformAllowedInEditor, bool usingWindowsPlatformInEditor) {
      showUnityEditorSettings = EditorGUILayout.Foldout(showUnityEditorSettings, "Unity Editor Settings");
      if (showUnityEditorSettings)
      {
        GUIHelper.HInset(6, () => {
          if(usingWindowsPlatformInEditor) {
            if (String.IsNullOrEmpty(PlatformSettings.AppID))
            {
              EditorGUILayout.HelpBox(string.Format(
                "Please enter a valid Oculus Rift App ID.",
                PlatformSettings.AppID), MessageType.Error);
            }
            else
            {
              EditorGUILayout.HelpBox(string.Format(
                "The Unity editor will connect to the Oculus platform and use the Rift app id ({0}).  The logged in user's data will be used.",
                PlatformSettings.AppID), MessageType.Info);
            }
          } else {
            if(String.IsNullOrEmpty(StandalonePlatformSettings.OculusPlatformAccessToken)) {
              EditorGUILayout.HelpBox(
                "Please enter a valid user token below.",
                MessageType.Error);
            } else {
              EditorGUILayout.HelpBox(
                "The Unity editor will use the supplied user token and operate in standalone mode.  Some user data will be mocked.",
                MessageType.Info);
            }
          }

          if (windowsPlatformAllowedInEditor)
          {
            PlatformSettings.UseStandalonePlatform = MakeToggle(useStandalonePlatformLabel, PlatformSettings.UseStandalonePlatform);
          }

          GUI.enabled = !usingWindowsPlatformInEditor;
          if (String.IsNullOrEmpty(StandalonePlatformSettings.OculusPlatformAccessToken))
          {
            if (GUILayout.Button("Get User Token"))
            {
              var appID = (string.IsNullOrEmpty(PlatformSettings.AppID)) ? PlatformSettings.MobileAppID : PlatformSettings.AppID;
              UnityEngine.Application.OpenURL("https://developer2.oculus.com/application/" + appID + "/api");
            }
          }
          StandalonePlatformSettings.OculusPlatformAccessToken = MakeTextBox(userTokenLabel, StandalonePlatformSettings.OculusPlatformAccessToken);
          GUI.enabled = true;
        });
      }
    }

    private void SetDirtyOnGUIChange()
    {
      if (GUI.changed)
      {
        EditorUtility.SetDirty(PlatformSettings.Instance);
        GUI.changed = false;
      }
    }
  }
}
