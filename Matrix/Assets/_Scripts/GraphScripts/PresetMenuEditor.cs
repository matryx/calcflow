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
    SerializedProperty s, px, py, pz, dz2, dxz, dyz, dxy, dx2y2, fz3, fxz2, fyz2, fxyz, fzx2y2, fxx23y2, fy3x2y2;

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

        //Orbitals
        s = serializedObject.FindProperty("s");
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
            EditorGUILayout.Space();

            EditorGUILayout.PropertyField(s);
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