using System.Collections;
using System.Collections.Generic;
using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif

#if UNITY_EDITOR
[CustomEditor(typeof(PresetMenu))]
public class PresetMenuEditor : Editor
{
    SerializedProperty flexMenu;
    SerializedProperty defaultFunction;
    SerializedProperty astroidalEllipse, bumpySphere, dinisSurface, figure8, graysSurface, knot, mobius, 
                       radialWave, torus;
    SerializedProperty cone, cube, cylinder, sphere, tetrahedron;
    SerializedProperty cinquefoilKnot, circle, sphereOutline, hypocloid, hypocloidSurface, trefoilKnot,
                       turnip, wavySurface, highResSphere;

    private void OnEnable()
    {
        flexMenu = serializedObject.FindProperty("menu");
        defaultFunction = serializedObject.FindProperty("defaultFunction");

        //R1 -> R1
        cinquefoilKnot = serializedObject.FindProperty("cinquefoilKnot");
        circle = serializedObject.FindProperty("circle");
        sphereOutline = serializedObject.FindProperty("sphereOutline");
        hypocloid = serializedObject.FindProperty("hypocloid");
        hypocloidSurface = serializedObject.FindProperty("hypocloidSurface");
        trefoilKnot = serializedObject.FindProperty("trefoilKnot");
        turnip = serializedObject.FindProperty("turnip");
        wavySurface = serializedObject.FindProperty("wavySurface");
        highResSphere = serializedObject.FindProperty("highResSphere");

        //R2 -> R3
        astroidalEllipse = serializedObject.FindProperty("astroidalEllipse");
        bumpySphere = serializedObject.FindProperty("bumpySphere");
        dinisSurface = serializedObject.FindProperty("dinisSurface");
        figure8 = serializedObject.FindProperty("figure8");
        graysSurface = serializedObject.FindProperty("graysSurface");
        knot = serializedObject.FindProperty("knot");
        mobius = serializedObject.FindProperty("mobius");
        radialWave = serializedObject.FindProperty("radialWave");
        torus = serializedObject.FindProperty("torus");

        //R3 -> R3
        cone = serializedObject.FindProperty("cone");
        cube = serializedObject.FindProperty("cube");
        cylinder = serializedObject.FindProperty("cylinder");
        sphere = serializedObject.FindProperty("sphere");
        tetrahedron = serializedObject.FindProperty("tetrahedron");
    }

    protected static bool presetsFold = true;

    public override void OnInspectorGUI()
    {
        GUIStyle foldoutStyle = new GUIStyle(EditorStyles.foldout);
        foldoutStyle.fontStyle = FontStyle.Bold;
        GUIStyle normalStyle = new GUIStyle(EditorStyles.toggle);
        normalStyle.fontStyle = FontStyle.Normal;

        PresetMenu presetMenu = (PresetMenu)target;
        serializedObject.Update();

        EditorGUILayout.PropertyField(flexMenu);
        EditorGUILayout.PropertyField(defaultFunction);

        presetsFold = EditorGUILayout.Foldout(presetsFold, "Select Presets", foldoutStyle);

        if (presetsFold)
        {
            EditorGUI.indentLevel++;
            EditorGUILayout.PropertyField(cinquefoilKnot);
            EditorGUILayout.PropertyField(circle);
            EditorGUILayout.PropertyField(sphereOutline);
            EditorGUILayout.PropertyField(hypocloid);
            EditorGUILayout.PropertyField(hypocloidSurface);
            EditorGUILayout.PropertyField(trefoilKnot);
            EditorGUILayout.PropertyField(turnip);
            EditorGUILayout.PropertyField(wavySurface);
            EditorGUILayout.PropertyField(highResSphere);
            EditorGUILayout.Space();

            EditorGUILayout.PropertyField(astroidalEllipse);
            EditorGUILayout.PropertyField(bumpySphere);
            EditorGUILayout.PropertyField(dinisSurface);
            EditorGUILayout.PropertyField(figure8);
            EditorGUILayout.PropertyField(graysSurface);
            EditorGUILayout.PropertyField(knot);
            EditorGUILayout.PropertyField(mobius);
            EditorGUILayout.PropertyField(radialWave);
            EditorGUILayout.PropertyField(torus);
            EditorGUILayout.Space();

            EditorGUILayout.PropertyField(cone);
            EditorGUILayout.PropertyField(cube);
            EditorGUILayout.PropertyField(cylinder);
            EditorGUILayout.PropertyField(sphere);
            EditorGUILayout.PropertyField(tetrahedron);
            EditorGUI.indentLevel--;
        }

        serializedObject.ApplyModifiedProperties();
    }
}
#endif