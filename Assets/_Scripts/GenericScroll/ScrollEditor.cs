using System.Collections;
using System.Collections.Generic;
using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
using UnityEditor.SceneManagement;
#endif
using UnityEngine.SceneManagement;

#if UNITY_EDITOR
[CustomEditor(typeof(Scroll))]
public class ScrollEditor : Editor
{
    SerializedProperty scrollerMaterial;
    SerializedProperty margin;
    SerializedProperty padding;

    SerializedProperty fixedRowOrCol;
    SerializedProperty numberOfVisibleThings;
    SerializedProperty objectParent;
    SerializedProperty movementSpeed;
    SerializedProperty fadeSpeed;
    SerializedProperty fadeInDelay;

    void OnEnable()
    {
        scrollerMaterial = serializedObject.FindProperty("scrollerMaterial");
        objectParent = serializedObject.FindProperty("objectParent");

        margin = serializedObject.FindProperty("margin");
        padding = serializedObject.FindProperty("padding");
        fixedRowOrCol = serializedObject.FindProperty("fixedRowOrCol");
        numberOfVisibleThings = serializedObject.FindProperty("numberOfVisibleThings");

        movementSpeed = serializedObject.FindProperty("movementSpeed");
        fadeSpeed = serializedObject.FindProperty("fadeSpeed");
        fadeInDelay = serializedObject.FindProperty("fadeInDelay");
    }

    protected static bool placementFold = true;
    protected static bool lerpSpeedFold = true;

    public override void OnInspectorGUI()
    {
        GUIStyle foldoutStyle = new GUIStyle(EditorStyles.foldout);
        foldoutStyle.fontStyle = FontStyle.Bold;

        Scroll scroll = (Scroll)target;
        serializedObject.Update();

        EditorGUILayout.PropertyField(scrollerMaterial);
        EditorGUILayout.PropertyField(objectParent);

        scroll.currOrientation =
            (Scroll.orientation)EditorGUILayout.EnumPopup("Orientation", scroll.currOrientation);

        placementFold = EditorGUILayout.Foldout(placementFold, "Object Placement", foldoutStyle);
        if (placementFold)
        {
            EditorGUI.indentLevel++;
            EditorGUILayout.PropertyField(margin);
            EditorGUILayout.PropertyField(padding);
            EditorGUILayout.PropertyField(fixedRowOrCol);
            EditorGUILayout.PropertyField(numberOfVisibleThings);
            EditorGUI.indentLevel--;
        }

        lerpSpeedFold = EditorGUILayout.Foldout(lerpSpeedFold, "Lerp Speed", foldoutStyle);
        if (lerpSpeedFold)
        {
            EditorGUI.indentLevel++;
            EditorGUILayout.PropertyField(movementSpeed);
            EditorGUILayout.PropertyField(fadeSpeed);
            EditorGUILayout.PropertyField(fadeInDelay);
            EditorGUI.indentLevel--;
        }

        serializedObject.ApplyModifiedProperties();
    }
}
#endif