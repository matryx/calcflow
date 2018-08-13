using System.Collections;
using System.Collections.Generic;
using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif

#if UNITY_EDITOR
//[CustomEditor(typeof(PresetMenu))]
// THIS FILE WILL BE DELETED, IT IS MERELY FOR QUICK BUILDING PURPOSES
[CustomEditor(typeof(DensityPresetMenu))]
public class DensityPresetMenuEditor : Editor
{
    SerializedProperty flexMenu;
    SerializedProperty defaultFunction;
    SerializedProperty s1, s2, p2, p3, d3z2, px, py, pz, dz2, dxz, dyz, dxy, dx2y2, fz3, fxz2, fyz2, fxyz, fzx2y2, fxx23y2, fy3x2y2;

    private void OnEnable()
    {
        flexMenu = serializedObject.FindProperty("menu");
        defaultFunction = serializedObject.FindProperty("defaultFunction");

        //Orbitals
        s1 = serializedObject.FindProperty("s1");
        s2 = serializedObject.FindProperty("s2");
        p2 = serializedObject.FindProperty("p2");
        p3 = serializedObject.FindProperty("p3");
        d3z2 = serializedObject.FindProperty("d3z2");
        px = serializedObject.FindProperty("px");
        py = serializedObject.FindProperty("py");
        pz = serializedObject.FindProperty("pz");
        dz2 = serializedObject.FindProperty("dz2");
        dxz = serializedObject.FindProperty("dxz");
        dyz = serializedObject.FindProperty("dyz");
        dxy = serializedObject.FindProperty("dxy");
        dx2y2 = serializedObject.FindProperty("dx2y2");
        fz3 = serializedObject.FindProperty("fz3");
        fxz2 = serializedObject.FindProperty("fxz2");
        fyz2 = serializedObject.FindProperty("fyz2");
        fxyz = serializedObject.FindProperty("fxyz");
        fzx2y2 = serializedObject.FindProperty("fzx2y2");
        fxx23y2 = serializedObject.FindProperty("fxx23y2");
        fy3x2y2 = serializedObject.FindProperty("fy3x2y2");
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
            EditorGUILayout.PropertyField(p2);
            EditorGUILayout.PropertyField(p3);
            EditorGUILayout.PropertyField(d3z2);
            EditorGUILayout.PropertyField(px);
            EditorGUILayout.PropertyField(py);
            EditorGUILayout.PropertyField(pz);
            EditorGUILayout.PropertyField(dz2);
            EditorGUILayout.PropertyField(dxz);
            EditorGUILayout.PropertyField(dyz);
            EditorGUILayout.PropertyField(dxy);
            EditorGUILayout.PropertyField(dx2y2);
            EditorGUILayout.PropertyField(fz3);
            EditorGUILayout.PropertyField(fxz2);
            EditorGUILayout.PropertyField(fyz2);
            EditorGUILayout.PropertyField(fxyz);
            EditorGUILayout.PropertyField(fzx2y2);
            EditorGUILayout.PropertyField(fxx23y2);
            EditorGUILayout.PropertyField(fy3x2y2);
            EditorGUI.indentLevel--;
        }

        serializedObject.ApplyModifiedProperties();
    }
}
#endif