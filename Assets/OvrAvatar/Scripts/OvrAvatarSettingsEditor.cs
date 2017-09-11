#if UNITY_EDITOR
using UnityEngine;
using System.Collections;
using UnityEditor;

[CustomEditor(typeof(OvrAvatarSettings))]
public class OvrAvatarSettingsEditor : Editor {
    GUIContent appIDLabel = new GUIContent("Oculus Rift App Id [?]", 
      "This AppID will be used for OvrAvatar registration.");

    GUIContent gearAppIDLabel = new GUIContent("Gear VR App Id [?]", 
      "This AppID will be used for OvrAvatar registration when building to the Android target.");

    [UnityEditor.MenuItem("Oculus Avatars/Edit Configuration")]
    public static void Edit()
    {
        var settings = OvrAvatarSettings.Instance;
        UnityEditor.Selection.activeObject = settings;
    }

    private static string MakeTextBox(GUIContent label, string variable) {
        EditorGUILayout.BeginHorizontal();
        EditorGUILayout.LabelField(label);
        GUI.changed = false;
        var result = EditorGUILayout.TextField(variable);
        if (GUI.changed)
        {
            EditorUtility.SetDirty(OvrAvatarSettings.Instance);
            GUI.changed = false;
        }
        EditorGUILayout.EndHorizontal();
        return result;
    }
    public override void OnInspectorGUI()
    {
        EditorGUILayout.BeginVertical();
        OvrAvatarSettings.AppID =
            OvrAvatarSettingsEditor.MakeTextBox(appIDLabel, OvrAvatarSettings.AppID);
        OvrAvatarSettings.GearAppID =
            OvrAvatarSettingsEditor.MakeTextBox(gearAppIDLabel, OvrAvatarSettings.GearAppID);
        EditorGUILayout.EndVertical();
    }
}
#endif
