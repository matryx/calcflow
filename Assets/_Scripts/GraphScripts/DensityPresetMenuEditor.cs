using System.Collections;
using System.Collections.Generic;
using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif

#if UNITY_EDITOR
//[CustomEditor(typeof(PresetMenu))]
[CustomEditor(typeof(DensityPresetMenu))]
public class DensityPresetMenuEditor : Editor
{
    SerializedProperty flexMenu;
    SerializedProperty defaultFunction;
    SerializedProperty s1, s2, p2, s3, p3, d3z2, d3xy, s4, p4, d4z2, d4xy, f4z3, f4xz2, f4xyz;

    private void OnEnable()
    {
        flexMenu = serializedObject.FindProperty("menu");
        defaultFunction = serializedObject.FindProperty("defaultFunction");

        //Orbitals
        s1 = serializedObject.FindProperty("s1");
        s2 = serializedObject.FindProperty("s2");
        p2 = serializedObject.FindProperty("p2");
        s3 = serializedObject.FindProperty("s3");
        p3 = serializedObject.FindProperty("p3");
        d3z2 = serializedObject.FindProperty("d3z2");
        d3xy = serializedObject.FindProperty("d3xy");
        s4 = serializedObject.FindProperty("s4");
        p4 = serializedObject.FindProperty("p4");
        d4z2 = serializedObject.FindProperty("d4z2");
        d4xy = serializedObject.FindProperty("d4xy");
        f4z3 = serializedObject.FindProperty("f4z3");
        f4xz2 = serializedObject.FindProperty("f4xz2");
        f4xyz = serializedObject.FindProperty("f4xyz");
    }

    protected static bool presetsFold = true;

    public override void OnInspectorGUI()
    {
        GUIStyle foldoutStyle = new GUIStyle(EditorStyles.foldout);
        foldoutStyle.fontStyle = FontStyle.Bold;
        GUIStyle normalStyle = new GUIStyle(EditorStyles.toggle);
        normalStyle.fontStyle = FontStyle.Normal;

        DensityPresetMenu presetMenu = (DensityPresetMenu)target;
        serializedObject.Update();

        EditorGUILayout.PropertyField(flexMenu);
        EditorGUILayout.PropertyField(defaultFunction);

        presetsFold = EditorGUILayout.Foldout(presetsFold, "Select Presets", foldoutStyle);

        if (presetsFold)
        {
            EditorGUI.indentLevel++;

            EditorGUILayout.PropertyField(s1);
            EditorGUILayout.PropertyField(s2);
            EditorGUILayout.PropertyField(s3);
            EditorGUILayout.PropertyField(p2);
            EditorGUILayout.PropertyField(p3);
            EditorGUILayout.PropertyField(d3z2);
            EditorGUILayout.PropertyField(d3xy);
            EditorGUILayout.PropertyField(s4);
            EditorGUILayout.PropertyField(p4);
            EditorGUILayout.PropertyField(d4z2);
            EditorGUILayout.PropertyField(d4xy);
            EditorGUILayout.PropertyField(f4z3);
            EditorGUILayout.PropertyField(f4xz2);
            EditorGUILayout.PropertyField(f4xyz);
            EditorGUI.indentLevel--;
        }

        serializedObject.ApplyModifiedProperties();
    }
}
#endif