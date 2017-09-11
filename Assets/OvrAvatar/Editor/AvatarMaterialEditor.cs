using UnityEngine;
using UnityEditor;
using NUnit.Framework;
using System.Linq;
using System.Collections.Generic;
using System;

public class AvatarMaterialEditor : MaterialEditor {

    static Dictionary<Material, int> layerVisibilityMasks = new Dictionary<Material, int>();
    const int MaxLayerCount = 8;

    private const string NormalMapPrefix = "NORMAL_MAP";
    private const string ParallaxPrefix = "PARALLAX";
    private const string RoughnessPrefix = "ROUGHNESS";
    private const string LayerKeywordPrefix = "LAYERS_";
	private const string AlphaMaskUniform = "_AlphaMask";
    private const string DarkMultUniform = "_DarkMultiplier";
	private const string BaseColorUniform = "_BaseColor";
    private const string BaseMaskTypeUniform = "_BaseMaskType";
    private const string BaseMaskParametersUniform = "_BaseMaskParameters";
    private const string BaseMaskAxisUniform = "_BaseMaskAxis";
    private const string NormalMapUniform = "_NormalMap";
    private const string ParallaxMapUniform = "_ParallaxMap";
    private const string RoughnessMapUniform = "_RoughnessMap";
    private const string LayerSurfacePrefix = "_LayerSurface";
    private const string LayerColorPrefix = "_LayerColor";
    private const string LayerSampleParametersPrefix = "_LayerSampleParameters";
    private const string LayerMaskTypePrefix = "_LayerMaskType";
    private const string LayerMaskParametersPrefix = "_LayerMaskParameters";
    private const string LayerMaskAxisPrefix = "_LayerMaskAxis";
    private const string LayerBlendModePrefix = "_LayerBlendMode";
    private const string LayerSampleModePrefix = "_LayerSampleMode";

    enum LayerSampleMode : int
    {
        Color,
        Texture,
        TextureSingleChannel,
        Parallax,
        RSRM,
        Count
    }
    static readonly string[] layerSampleModeLabels = new string[(int)LayerSampleMode.Count] { "Color", "Texture", "Texture (single channel)", "Parallax", "RSRM" };

    enum LayerMaskType : int
    {
        None,
        Positional,
        ViewReflection,
        Fresnel,
        Pulse,
        Count
    }
    static readonly string[] layerMaskTypeLabels = new string[(int)LayerMaskType.Count] { "None", "Positional", "View reflection", "Fresnel", "Pulse" };

    enum LayerBlendMode : int
    {
        Add,
        Multiply,
        Count
    }
    static readonly string[] layerBlendModeLabels = new string[(int)LayerBlendMode.Count] { "Add", "Multiply" };

    Material[] previewMaterials = new Material[MaxLayerCount];
    Vector4[,] sampleParametersCache = new Vector4[MaxLayerCount, (int)LayerSampleMode.Count];
    Vector4[][] maskParametersCache = new Vector4[MaxLayerCount][];
    Vector4[][] maskAxisCache = new Vector4[MaxLayerCount][];
    Vector4[] baseMaskParametersCache = new Vector4[(int)LayerMaskType.Count];
    Vector4[] baseMaskAxisCache = new Vector4[(int)LayerMaskType.Count];
    PreviewRenderUtility previewUtility;
    Mesh previewMesh;

    static readonly Vector4 PositionalMaskDefaults = new Vector4(0.0f, 1.0f, 1.0f, 0.0f);
    static readonly Vector4 ViewReflectionMaskDefaults = new Vector4(1.0f, 0.0f, 1.0f, 0.0f);
    static readonly Vector4 FresnelMaskDefaults = new Vector4(1.0f, 0.0f, 1.0f, 1.0f);
    static readonly Vector4 PulseMaskDefaults = new Vector4(1.0f, 0.0f, 1.0f, 0.0f);
    static readonly Vector4 MaskAxisDefault = new Vector4(0.0f, 1.0f, 0.0f, 0.0f);

    void Init()
    {
        if (previewUtility == null)
        {
            previewUtility = new PreviewRenderUtility();
            GameObject gameObject = (GameObject)EditorGUIUtility.LoadRequired("Previews/PreviewMaterials.fbx");
            previewMesh = gameObject.transform.Find("sphere").GetComponent<MeshFilter>().sharedMesh;
        }

        baseMaskParametersCache[(int)LayerMaskType.Positional] = PositionalMaskDefaults;
        baseMaskParametersCache[(int)LayerMaskType.ViewReflection] = ViewReflectionMaskDefaults;
        baseMaskParametersCache[(int)LayerMaskType.Fresnel] = FresnelMaskDefaults;
        baseMaskParametersCache[(int)LayerMaskType.Pulse] = PulseMaskDefaults;
        
        baseMaskAxisCache[(int)LayerMaskType.Positional] = MaskAxisDefault;
        baseMaskAxisCache[(int)LayerMaskType.ViewReflection] = MaskAxisDefault;

        for (int i = 0; i < MaxLayerCount; ++i)
        {
            sampleParametersCache[i, (int)LayerSampleMode.Color] = new Vector4(1.0f, 1.0f, 1.0f, 1.0f);
            sampleParametersCache[i, (int)LayerSampleMode.Texture] = new Vector4(0.0f, 0.0f, 1.0f, 1.0f);
            sampleParametersCache[i, (int)LayerSampleMode.TextureSingleChannel] = new Vector4(1.0f, 0.0f, 0.0f, 0.0f);
            sampleParametersCache[i, (int)LayerSampleMode.Parallax] = new Vector4(0.0f, 1.0f, 0.0f, 0.0f);
            sampleParametersCache[i, (int)LayerSampleMode.RSRM] = new Vector4(0.0f, 1.0f, 1.0f, 0.0f);

            Vector4[] parametersCache = new Vector4[(int)LayerMaskType.Count];
            parametersCache[(int)LayerMaskType.Positional] = PositionalMaskDefaults;
            parametersCache[(int)LayerMaskType.ViewReflection] = ViewReflectionMaskDefaults;
            parametersCache[(int)LayerMaskType.Fresnel] = FresnelMaskDefaults;
            parametersCache[(int)LayerMaskType.Pulse] = PulseMaskDefaults;
            maskParametersCache[i] = parametersCache;

            Vector4[] axisCache = new Vector4[(int)LayerMaskType.Count];
            axisCache[(int)LayerMaskType.Positional] = MaskAxisDefault;
            axisCache[(int)LayerMaskType.ViewReflection] = MaskAxisDefault;
            axisCache[(int)LayerMaskType.Pulse] = MaskAxisDefault;
            maskAxisCache[i] = axisCache;
        }
    }

    static class AvatarMaterialEditorGUILayout
    {
        public static bool KeywordToggle(string label, Material material, string keywordPrefix)
        {
            bool isEnabled = material.IsKeywordEnabled(keywordPrefix + "_ON");
            bool newIsEnabled = EditorGUILayout.Toggle(label, isEnabled);
            if (newIsEnabled != isEnabled)
            {
                Undo.RecordObject(material, label + " change");
                if (newIsEnabled)
                {
                    material.EnableKeyword(keywordPrefix + "_ON");
                    material.DisableKeyword(keywordPrefix + "_OFF");
                }
                else
                {
                    material.EnableKeyword(keywordPrefix + "_OFF");
                    material.DisableKeyword(keywordPrefix + "_ON");
                }
            }
            return newIsEnabled;
        }

        public static Color ColorField(string label, Material material, string propertyName)
        {
            Color color = material.GetColor(propertyName);
            Color newColor = EditorGUILayout.ColorField(label, color);
            if (newColor != color)
            {
                Undo.RecordObject(material, label + " change");
                material.SetColor(propertyName, newColor);
            }
            return newColor;
        }

        internal static int IntField(string label, Material material, string propertyName, string[] valueNames) 
        {
            int currentValue = material.GetInt(propertyName);
            int newValue = EditorGUILayout.Popup(label, currentValue, valueNames);
            if (newValue != currentValue)
            {
                Undo.RecordObject(material, label + " change");
                material.SetInt(propertyName, newValue);
            }
            return newValue;
        }

        internal static Vector2 Vector2Field(string label, Material material, string propertyName)
        {
            Vector4 currentValue = material.GetVector(propertyName);
            Vector2 currentVec2 = new Vector2(currentValue.x, currentValue.y);
            Vector2 newVec2 = EditorGUILayout.Vector2Field(label, currentVec2);
            if (newVec2 != currentVec2)
            {
                Undo.RecordObject(material, label + " change");
                material.SetVector(propertyName, new Vector4(newVec2.x, newVec2.y, currentValue.z, currentValue.w));
            }
            return newVec2;
        }

        internal static Vector3 Vector3Field(string label, Material material, string propertyName)
        {
            Vector4 currentValue = material.GetVector(propertyName);
            Vector3 currentVec3 = new Vector3(currentValue.x, currentValue.y, currentValue.z);
            Vector3 newVec3 = EditorGUILayout.Vector3Field(label, currentVec3);
            if (newVec3 != currentVec3)
            {
                Undo.RecordObject(material, label + " change");
                material.SetVector(propertyName, new Vector4(newVec3.x, newVec3.y, newVec3.z, currentValue.w));
            }
            return newVec3;
        }

        internal static float VectorComponentField(string label, Material material, string propertyName, int componentIndex)
        {
            Vector4 currentValue = material.GetVector(propertyName);
            float currentComponent = currentValue[componentIndex];
            float newComponent = EditorGUILayout.FloatField(label, currentComponent);
            if (newComponent != currentComponent)
            {
                Undo.RecordObject(material, label + " change");
                currentValue[componentIndex] = newComponent;
                material.SetVector(propertyName, currentValue);
            }
            return newComponent;
        }

        internal static Vector4 ChannelMaskField(string label, Material material, string propertyName)
        {
            Vector4 currentValue = material.GetVector(propertyName);
            int currentChannel = 0;
            for (int i = 0; i < 4; ++i)
            {
                if (currentValue[i] != 0)
                {
                    currentChannel = i;
                    break;
                }
            }
            int newChannel = EditorGUILayout.IntPopup(label, currentChannel, new string[] { "R", "G", "B", "A" }, new int[] { 0, 1, 2, 3 });
            if (newChannel != currentChannel)
            {
                Vector4 channelMask = Vector4.zero;
                channelMask[newChannel] = 1.0f;
                Undo.RecordObject(material, label + " change");
                currentValue = channelMask;
                material.SetVector(propertyName, currentValue);
            }
            return currentValue;
        }
    }

    void MaskField(
        string label,
        Material material,
        string maskTypePropertyName,
        string maskParametersPropertyName,
        string maskAxisPropertyName,
        Vector4[] axisCache,
        Vector4[] parameterCache,
        bool normalMapEnabled)
    {
        EditorGUI.BeginChangeCheck();
        LayerMaskType maskType = (LayerMaskType)AvatarMaterialEditorGUILayout.IntField(label, material, maskTypePropertyName, layerMaskTypeLabels);
        if (EditorGUI.EndChangeCheck())
        {
            material.SetVector(maskAxisPropertyName, axisCache[(int)maskType]);
            material.SetVector(maskParametersPropertyName, parameterCache[(int)maskType]);
        }

        // Show mask-specific controls
        EditorGUI.BeginChangeCheck();
        switch (maskType)
        {
            case LayerMaskType.Positional:
                AvatarMaterialEditorGUILayout.Vector3Field("Axis", material, maskAxisPropertyName);
                AvatarMaterialEditorGUILayout.VectorComponentField("Center distance", material, maskParametersPropertyName, 0);
                AvatarMaterialEditorGUILayout.VectorComponentField("Fade above", material, maskParametersPropertyName, 1);
                AvatarMaterialEditorGUILayout.VectorComponentField("Fade below", material, maskParametersPropertyName, 2);
                break;
            case LayerMaskType.ViewReflection:
                AvatarMaterialEditorGUILayout.Vector3Field("Axis", material, maskAxisPropertyName);
                AvatarMaterialEditorGUILayout.VectorComponentField("Fade begin", material, maskParametersPropertyName, 0);
                AvatarMaterialEditorGUILayout.VectorComponentField("Fade end", material, maskParametersPropertyName, 1);
                if (normalMapEnabled)
                {
                    AvatarMaterialEditorGUILayout.VectorComponentField("Normal map strength", material, maskParametersPropertyName, 2);
                }
                break;
            case LayerMaskType.Fresnel:
                AvatarMaterialEditorGUILayout.VectorComponentField("Power", material, maskParametersPropertyName, 0);
                AvatarMaterialEditorGUILayout.VectorComponentField("Fade begin", material, maskParametersPropertyName, 1);
                AvatarMaterialEditorGUILayout.VectorComponentField("Fade end", material, maskParametersPropertyName, 2);
                if (normalMapEnabled)
                {
                    AvatarMaterialEditorGUILayout.VectorComponentField("Normal map strength", material, maskParametersPropertyName, 3);
                }
                break;
            case LayerMaskType.Pulse:
                AvatarMaterialEditorGUILayout.Vector3Field("Axis", material, maskAxisPropertyName);
                AvatarMaterialEditorGUILayout.VectorComponentField("Pulse distance", material, maskParametersPropertyName, 0);
                AvatarMaterialEditorGUILayout.VectorComponentField("Pulse speed", material, maskParametersPropertyName, 1);
                AvatarMaterialEditorGUILayout.VectorComponentField("Power", material, maskParametersPropertyName, 2);
                break;
        }
        if (EditorGUI.EndChangeCheck())
        {
            parameterCache[(int)maskType] = material.GetVector(maskParametersPropertyName);
            axisCache[(int)maskType] = material.GetVector(maskAxisPropertyName);
        }
    }

    public override void OnInspectorGUI()
    {
        Init();
        if (!isVisible)
        {
            return;
        }
        SetDefaultGUIWidths();

        Material material = target as Material;
        int layerCount = MaxLayerCount;
        while (layerCount > 0 && !material.IsKeywordEnabled(GetPropertyName(LayerKeywordPrefix, layerCount)))
        {
            --layerCount;
        }
        if (layerCount == 0)
        {
            layerCount = 1;
            material.EnableKeyword("LAYERS_" + layerCount);
        }

        EditorGUILayout.LabelField("Global material properties");
        EditorGUILayout.BeginVertical(EditorStyles.helpBox);
		TextureField("AlphaMask", material, AlphaMaskUniform);
        AvatarMaterialEditorGUILayout.ColorField("DarkMultiplier", material, DarkMultUniform);
		AvatarMaterialEditorGUILayout.ColorField("BaseColor", material, BaseColorUniform);
        bool normalMapEnabled = AvatarMaterialEditorGUILayout.KeywordToggle("Normal map enabled", material, NormalMapPrefix);
        if (normalMapEnabled)
        {
            TextureField("Normal map", material, NormalMapUniform);
        }
        bool parallaxEnabled = AvatarMaterialEditorGUILayout.KeywordToggle("Parallax enabled", material, ParallaxPrefix);
        if (parallaxEnabled)
        {
            TextureField("Parallax map", material, ParallaxMapUniform);
        }
        bool roughnessEnabled = AvatarMaterialEditorGUILayout.KeywordToggle("Roughness enabled", material, RoughnessPrefix);
        if (roughnessEnabled)
        {
            TextureField("Roughness map", material, RoughnessMapUniform);
        }

        MaskField("Base mask type", material, BaseMaskTypeUniform, BaseMaskParametersUniform, BaseMaskAxisUniform, baseMaskAxisCache, baseMaskParametersCache, normalMapEnabled);

        EditorGUILayout.EndVertical();

        int layerVisibilityMask = 0;
        layerVisibilityMasks.TryGetValue(material, out layerVisibilityMask);

        EditorGUILayout.LabelField("Layers");
        EditorGUI.indentLevel++;
        EditorGUILayout.BeginVertical(EditorStyles.helpBox);
        int? removeLayerIndex = null;
        int? swapSource = null;
        int? swapTarget = null;
        for (int i = 0; i < layerCount; ++i)
        {

            // Draw the preview material
            if (previewMaterials[i] == null)
                previewMaterials[i] = CreatePreviewMaterial(material, i);

            const int previewSize = 64;
            const int buttonSize = 20;
            Rect layerHeaderRect = GUILayoutUtility.GetRect(previewSize, previewSize, GUILayout.ExpandWidth(true));

            // Draw the preview texture
            previewUtility.m_Camera.transform.position = Vector3.forward * 5.0f;
            previewUtility.m_Camera.transform.rotation = Quaternion.identity;
            previewUtility.m_Camera.transform.LookAt(Vector3.zero);
            previewUtility.BeginStaticPreview(new Rect(0, 0, previewSize, previewSize));
            previewUtility.DrawMesh(previewMesh, Vector3.zero, Quaternion.identity, previewMaterials[i], 0);
            previewUtility.m_Camera.Render();
            Texture preview = previewUtility.EndStaticPreview();
            GUI.Label(new Rect(layerHeaderRect.xMax - previewSize - buttonSize, layerHeaderRect.y, previewSize, previewSize), preview);

            float yButton = layerHeaderRect.y;
            EditorGUI.BeginDisabledGroup(layerCount <= 1);
            if (GUI.Button(new Rect(layerHeaderRect.xMax - buttonSize, yButton, buttonSize, buttonSize), new GUIContent("X", "Remove layer")))
            {
                removeLayerIndex = i;
            }
            yButton += buttonSize + 4;
            EditorGUI.EndDisabledGroup();

            EditorGUI.BeginDisabledGroup(i == 0);
            if (GUI.Button(new Rect(layerHeaderRect.xMax - buttonSize, yButton, buttonSize, buttonSize), new GUIContent("^", "Move layer up")))
            {
                swapSource = i;
                swapTarget = i - 1;
            }
            yButton += buttonSize;
            EditorGUI.EndDisabledGroup();

            EditorGUI.BeginDisabledGroup(i == layerCount - 1);
            if (GUI.Button(new Rect(layerHeaderRect.xMax - buttonSize, yButton, buttonSize, buttonSize), new GUIContent("v", "Move layer down")))
            {
                swapSource = i;
                swapTarget = i + 1;
            }
            yButton += buttonSize;
            EditorGUI.EndDisabledGroup();

            // Create a toggleable group for the layer
            int layerMaskBit = 1 << i;
            bool layerVisible = (layerVisibilityMask & layerMaskBit) != 0;
            layerVisible = EditorGUI.Foldout(layerHeaderRect, layerVisible, string.Format("Layer {0}", i + 1));

            if (layerVisible)
                layerVisibilityMask |= layerMaskBit;
            else
                layerVisibilityMask &= ~layerMaskBit;
            if (layerVisible)
            {
                EditorGUILayout.BeginVertical(EditorStyles.helpBox);
                EditorGUI.BeginChangeCheck();
                {
                    // Handle the blend mode
                    AvatarMaterialEditorGUILayout.IntField("Blend mode", material, GetPropertyName(LayerBlendModePrefix, i), layerBlendModeLabels);

                    // Handle the sample mode selector
                    string layerSampleParametersProperty = GetPropertyName(LayerSampleParametersPrefix, i);
                    EditorGUI.BeginChangeCheck();
                    LayerSampleMode sampleMode = (LayerSampleMode)AvatarMaterialEditorGUILayout.IntField("Sample mode", material, GetPropertyName(LayerSampleModePrefix, i), layerSampleModeLabels);
                    if (EditorGUI.EndChangeCheck())
                    {
                        material.SetVector(layerSampleParametersProperty, sampleParametersCache[i, (int)sampleMode]);
                    }

                    // Show the mode-specific sample controls
                    EditorGUI.BeginChangeCheck();
                    AvatarMaterialEditorGUILayout.ColorField("Surface color", material, GetPropertyName(LayerColorPrefix, i));
                    switch (sampleMode) {
                        case LayerSampleMode.Texture:
                            TextureField("Surface texture", material, GetPropertyName(LayerSurfacePrefix, i));
                            AvatarMaterialEditorGUILayout.Vector2Field("Panning speed", material, layerSampleParametersProperty);
                            break;
                        case LayerSampleMode.TextureSingleChannel:
                            TextureField("Surface texture", material, GetPropertyName(LayerSurfacePrefix, i));
                            AvatarMaterialEditorGUILayout.ChannelMaskField("Channel", material, layerSampleParametersProperty);
                            break;
                        case LayerSampleMode.Parallax:
                            TextureField("Surface texture", material, GetPropertyName(LayerSurfacePrefix, i));
                            AvatarMaterialEditorGUILayout.VectorComponentField("Parallax min height", material, layerSampleParametersProperty, 0);
                            AvatarMaterialEditorGUILayout.VectorComponentField("Parallax max height", material, layerSampleParametersProperty, 1);
                            break;
                        case LayerSampleMode.RSRM:
                            TextureField("RSRM texture", material, GetPropertyName(LayerSurfacePrefix, i));
                            if (roughnessEnabled)
                            {
                                AvatarMaterialEditorGUILayout.VectorComponentField("Roughness min", material, layerSampleParametersProperty, 0);
                                AvatarMaterialEditorGUILayout.VectorComponentField("Roughness max", material, layerSampleParametersProperty, 1);
                            }
                            else
                            {
                                AvatarMaterialEditorGUILayout.VectorComponentField("Roughness", material, layerSampleParametersProperty, 0);
                            }
                            if (normalMapEnabled)
                            {
                                AvatarMaterialEditorGUILayout.VectorComponentField("Normal map strength", material, layerSampleParametersProperty, 2);
                            }
                            break;
                    }
                    if (EditorGUI.EndChangeCheck())
                    {
                        sampleParametersCache[i, (int)sampleMode] = material.GetVector(layerSampleParametersProperty);
                    }

                    // Handle the mask mode selector
                    string maskParametersName = GetPropertyName(LayerMaskParametersPrefix, i);
                    string maskAxisName = GetPropertyName(LayerMaskAxisPrefix, i);
                    MaskField("Mask Type", material, GetPropertyName(LayerMaskTypePrefix, i), maskParametersName, maskAxisName, maskAxisCache[i], maskParametersCache[i], normalMapEnabled);
                }
                if (EditorGUI.EndChangeCheck())
                {
                    previewMaterials[i] = null;
                }
                EditorGUILayout.EndVertical();
            }
        }
        layerVisibilityMasks[material] = layerVisibilityMask;

        if (layerCount < MaxLayerCount)
        {
            if (GUILayout.Button("Add layer"))
            {
                Undo.RecordObject(material, "Add layer");
                SetLayerCount(material, layerCount + 1);
                removeLayerIndex = null;
            }
        }
        if (removeLayerIndex.HasValue)
        {
            Undo.RecordObject(material, "Remove layer");
            for (int i = removeLayerIndex.Value; i < layerCount - 1; ++i)
            {
                CopyAttributes(material, i + 1, i);
            }
            SetLayerCount(material, layerCount - 1);
        }
        if (swapSource.HasValue && swapTarget.HasValue)
        {
            Undo.RecordObject(material, string.Format("Swap layers {1} and {0}", swapSource.Value, swapTarget.Value));
            SwapAttributes(material, swapSource.Value, swapTarget.Value);
        }

        EditorGUI.indentLevel--;
        EditorGUILayout.EndVertical();
        EditorUtility.SetDirty(target);
    }

    public Texture TextureField(string label, Material material, string propertyName)
    {
        Texture texture = material.GetTexture(propertyName);
        MaterialProperty property = MaterialEditor.GetMaterialProperty(new[] { material }, propertyName);
        Texture newTexture = base.TextureProperty(property, label);
        if (newTexture != texture)
        {
            Undo.RecordObject(material, label + " change");
            material.SetTexture(propertyName, newTexture);
        }
        return newTexture;
    }

    private Material CreatePreviewMaterial(Material material, int layerIndex)
    {
        Material previewMaterial = new Material(material);
        CopyAttributes(previewMaterial, layerIndex, 0);
        SetLayerCount(previewMaterial, 1);
        previewMaterial.SetVector(DarkMultUniform, new Vector4(0.6f, 0.6f, 0.6f, 1.0f));
		previewMaterial.SetVector(BaseColorUniform, new Vector4(0.0f, 0.0f, 0.0f, 1.0f));
        previewMaterial.SetTexture(AlphaMaskUniform, EditorGUIUtility.whiteTexture);
        return previewMaterial;
    }

    private void SetLayerCount(Material material, int layerCount)
    {
        for (int i = 0; i < MaxLayerCount; ++i)
        {
            string layerCountProperty = GetPropertyName("LAYERS_", i + 1);
            if (i + 1 == layerCount)
                material.EnableKeyword(layerCountProperty);
            else
                material.DisableKeyword(layerCountProperty);
        }
    }

    private static string GetPropertyName(string baseName, int index)
    {
        return string.Format("{0}{1}", baseName, index);
    }

    class LayerAttributes
    {
        public Texture surface { get; private set; }
        public Color color { get; private set; }
        public LayerMaskType maskType { get; private set; }
        public Vector4 maskParameters { get; private set; }
        public Vector4 maskAxis { get; private set; }
        public LayerSampleMode sampleMode { get; private set; }
        public Vector4 sampleParameters { get; private set; }
        public LayerBlendMode blendMode { get; private set; }
        public LayerAttributes()
        {
            this.surface = null;
            this.color = Color.white;
            this.maskType = (int)LayerMaskType.None;
            this.maskParameters = Vector4.zero;
            this.maskAxis = new Vector4(0.0f, 1.0f, 0.0f, 0.0f);
            this.sampleMode = (int)LayerSampleMode.Color;
            this.sampleParameters = new Vector4(1.0f, 1.0f, 1.0f, 1.0f);
            this.blendMode = (int)LayerBlendMode.Add;
        }
        public LayerAttributes(
            Texture surface,
            Color color,
            LayerMaskType maskType,
            Vector3 maskParameters,
            Vector4 maskAxis,
            LayerSampleMode sampleMode,
            Vector4 sampleParameters,
            LayerBlendMode blendMode
        ) {
            this.surface = surface;
            this.color = color;
            this.maskType = maskType;
            this.maskParameters = maskParameters;
            this.maskAxis = maskAxis;
            this.sampleMode = sampleMode;
            this.sampleParameters = sampleParameters;
            this.blendMode = blendMode;
        }
    }

    private void CopyAttributes(Material material, int sourceIndex, int targetIndex)
    {
        LayerAttributes attributes = GetLayerAttributes(material, sourceIndex);
        SetLayerAttributes(material, targetIndex, attributes);
        previewMaterials[targetIndex] = null;
    }

    private void SwapAttributes(Material material, int sourceIndex, int targetIndex)
    {
        LayerAttributes sourceAttributes = GetLayerAttributes(material, sourceIndex);
        LayerAttributes targetAttributes = GetLayerAttributes(material, targetIndex);
        SetLayerAttributes(material, sourceIndex, targetAttributes);
        SetLayerAttributes(material, targetIndex, sourceAttributes);
        previewMaterials[sourceIndex] = null;
        previewMaterials[targetIndex] = null;
    }

    private static LayerAttributes GetLayerAttributes(Material material, int layerIndex)
    {
        return new LayerAttributes(
            material.GetTexture(GetPropertyName(LayerSurfacePrefix, layerIndex)),
            material.GetColor(GetPropertyName(LayerColorPrefix, layerIndex)),
            (LayerMaskType)material.GetInt(GetPropertyName(LayerMaskTypePrefix, layerIndex)),
            material.GetVector(GetPropertyName(LayerMaskParametersPrefix, layerIndex)),
            material.GetVector(GetPropertyName(LayerMaskAxisPrefix, layerIndex)),
            (LayerSampleMode)material.GetInt(GetPropertyName(LayerSampleModePrefix, layerIndex)),
            material.GetVector(GetPropertyName(LayerSampleParametersPrefix, layerIndex)),
            (LayerBlendMode)material.GetInt(GetPropertyName(LayerBlendModePrefix, layerIndex))
        );
    }

    private static void SetLayerAttributes(Material material, int layerIndex, LayerAttributes attributes)
    {
        material.SetTexture(GetPropertyName(LayerSurfacePrefix, layerIndex), attributes.surface);
        material.SetColor(GetPropertyName(LayerColorPrefix, layerIndex), attributes.color);
        material.SetInt(GetPropertyName(LayerMaskTypePrefix, layerIndex), (int)attributes.maskType);
        material.SetVector(GetPropertyName(LayerMaskParametersPrefix, layerIndex), attributes.maskParameters);
        material.SetVector(GetPropertyName(LayerMaskAxisPrefix, layerIndex), attributes.maskAxis);
        material.SetInt(GetPropertyName(LayerSampleModePrefix, layerIndex), (int)attributes.sampleMode);
        material.SetVector(GetPropertyName(LayerSampleParametersPrefix, layerIndex), attributes.sampleParameters);
        material.SetInt(GetPropertyName(LayerBlendModePrefix, layerIndex), (int)attributes.blendMode);
    }
}
