///////////////////////////////////////////////
// MKGlowSystem	Editor						 //
//											 //
// Created by Michael Kremmel on 23.12.2014  //
// Copyright © 2015 All rights reserved.     //
///////////////////////////////////////////////
#if UNITY_EDITOR
using UnityEngine;
using System.Collections;
using UnityEditor;

public class MKGlowSystemHelpWindow : EditorWindow
{
    private static MKGlowSystemHelpWindow window;
    //private static Texture2D m_SettingsLabel;
    private const string m_Style = "box";

    [MenuItem("Window/MKGlowSystem/Help  |  FAQ")]
    static void Init()
    {
        window = (MKGlowSystemHelpWindow)EditorWindow.GetWindow<MKGlowSystemHelpWindow>("MKGlow Help");
        window.minSize = new Vector2(664, 325);
        window.maxSize = new Vector2(664, 325);
		/*
        var filePath = Application.dataPath + "/_MK/_MKGlowSystem/_Internal/Editor/_Image/MKGlowSystemHelpTitle.png";
        if (System.IO.File.Exists(filePath))
        {
            var bytes = System.IO.File.ReadAllBytes(filePath);
            if (m_SettingsLabel == null)
            {
                m_SettingsLabel = new Texture2D(128, 128);
                m_SettingsLabel.LoadImage(bytes);
            }
        }*/

        window.Show();
    }

    private void OnGUI()
    {
		/*
        if(m_SettingsLabel != null)
        {
            GUILayout.BeginHorizontal(m_Style);
            GUILayout.FlexibleSpace();
            GUILayout.Label(m_SettingsLabel);
            GUILayout.FlexibleSpace();
            GUILayout.EndHorizontal();
        }
		*/

        GUILayout.BeginVertical(m_Style);
        GUI.contentColor = Color.green;
        EditorGUILayout.LabelField("Contact: ");
        GUI.contentColor = Color.white;
        EditorGUILayout.LabelField("E-Mail: mkremmel@gmx.de - Feel free to contact me if you have any problems or ideas for the plugin");
        EditorGUILayout.LabelField("Web: www.michaelkremmel.de");
        GUILayout.EndVertical();


        GUILayout.BeginVertical(m_Style);
        GUI.contentColor = Color.green;
        EditorGUILayout.LabelField("Settings: ");
        GUI.contentColor = Color.white;

        EditorGUILayout.LabelField("Glow Render Layer        - Renderlayer that should glow (only selective glow)");
        EditorGUILayout.LabelField("Glow Resolution            - The resolution of the rendered glow");
        EditorGUILayout.LabelField("Glow Type                    - Selective = to specifically bring objects to glow, Fullscreen = complete screen glows");
        EditorGUILayout.LabelField("Glow Tint                      - The glows coloration in full screen mode (only FullscreenGlowType)");
        EditorGUILayout.LabelField("Glow Quality                 - The main difference between Low and High is that Low has less Garbage Collection");
        EditorGUILayout.LabelField("Glow Curve                  - The Glows blur calculation");
        EditorGUILayout.LabelField("Blur Spread                  - Width of the glow effect");
        EditorGUILayout.LabelField("Blur Iterations               - Number of used blurs");
        EditorGUILayout.LabelField("Blur Offset                    - Distance to the object per blur");
        EditorGUILayout.LabelField("Blur Samples                - Significantly influences the blurs quality (recommended: 4)");
        EditorGUILayout.LabelField("Glow Intensity               - The global luminous intensity");
        EditorGUILayout.LabelField("Show Transparent Glow - Show glow through Transparent rendered objects");
        EditorGUILayout.LabelField("Show Cutout Glow         - Show glow through Cutout rendered objects");
        GUILayout.EndVertical();
    }
}
#endif