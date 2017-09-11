///////////////////////////////////////////////
// MKGlowSystem	Editor						 //
//											 //
// Created by Michael Kremmel on 23.12.2014  //
// Copyright © 2015 All rights reserved.     //
///////////////////////////////////////////////

#if UNITY_EDITOR
using UnityEngine;
using UnityEditor;
using System;
using System.Reflection;
using MKGlowSystem;

namespace MKGlowSystemEditor
{
    [CustomEditor(typeof(MKGlow))]
    public class MKGlowEditor : Editor
    {
        private const string m_Style = "box";
        //private static Texture2D m_ComponentLabel;

        private SerializedProperty glowCurve;
        private SerializedProperty samples;
        private SerializedProperty blurSpread;
        private SerializedProperty blurIterations;
        private SerializedProperty blurOffset;
        private SerializedProperty glowIntensity;
        private SerializedProperty glowQuality;
        private SerializedProperty glowType;
        private SerializedProperty glowMode;
        private SerializedProperty fullScreenGlowTint;
        private SerializedProperty showTransparent;
        private SerializedProperty showCutout;
        private SerializedProperty glowLayer;
        //private bool helpToggle = false;

		[MenuItem("Window/MKGlowSystem/Add MK Glow System To Selection")]
		private static void AddMKGlowToObject()
		{
			foreach (GameObject obj in Selection.gameObjects)
			{
				if (obj.GetComponent<MKGlow>() == null)
					obj.AddComponent<MKGlow>();
			}
		}

        private void OnEnable()
        {
            glowCurve = serializedObject.FindProperty("m_GlowCurve");
            samples = serializedObject.FindProperty("m_Samples");
            blurSpread = serializedObject.FindProperty("m_BlurSpread");
            blurIterations = serializedObject.FindProperty("m_BlurIterations");
            blurOffset = serializedObject.FindProperty("m_BlurOffset");
            glowIntensity = serializedObject.FindProperty("m_GlowIntensity");
            glowQuality = serializedObject.FindProperty("m_GlowQuality");
            glowType = serializedObject.FindProperty("m_GlowType");
            glowMode = serializedObject.FindProperty("m_GlowResolution");
            fullScreenGlowTint = serializedObject.FindProperty("m_FullScreenGlowTint");
            showTransparent = serializedObject.FindProperty("m_ShowTransparentGlow");
            showCutout = serializedObject.FindProperty("m_ShowCutoutGlow");
            glowLayer = serializedObject.FindProperty("m_GlowRenderLayer");
        }

        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            //GUILayout.BeginVertical(m_Style);
            if (glowType.intValue == 0)
            {
                EditorGUILayout.PropertyField(glowLayer);
            }
            EditorGUILayout.PropertyField(glowMode);
            EditorGUILayout.PropertyField(glowType);
            EditorGUILayout.PropertyField(glowQuality);
            EditorGUILayout.PropertyField(glowCurve);

            if (glowType.intValue == 1)
            {
                fullScreenGlowTint.colorValue = EditorGUILayout.ColorField("Glow Tint", fullScreenGlowTint.colorValue);
            }

            EditorGUILayout.Slider(blurSpread, 0.2f, 2f, "Blur Spread");
            EditorGUILayout.IntSlider(blurIterations, 0, 11, "Blur Iterations");
            EditorGUILayout.Slider(blurOffset, 0f, 4f, "Blur Offset");
            EditorGUILayout.IntSlider(samples, 2, 16, "Blur Samples");
            EditorGUILayout.Slider(glowIntensity, 0f, 1f, "Glow Intensity");
            EditorGUILayout.PropertyField(showTransparent);
            EditorGUILayout.PropertyField(showCutout);

            serializedObject.ApplyModifiedProperties();

            //DrawDefaultInspector ();

        }
    }
}
#endif