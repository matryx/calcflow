using System;
using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using Oculus.Avatar;
using System.Threading;

public class OvrAvatarComponent : MonoBehaviour
{

    public static readonly string[] LayerKeywords = new[] { "LAYERS_0", "LAYERS_1", "LAYERS_2", "LAYERS_3", "LAYERS_4", "LAYERS_5", "LAYERS_6", "LAYERS_7", "LAYERS_8", };
    public static readonly string[] LayerSampleModeParameters = new[] { "_LayerSampleMode0", "_LayerSampleMode1", "_LayerSampleMode2", "_LayerSampleMode3", "_LayerSampleMode4", "_LayerSampleMode5", "_LayerSampleMode6", "_LayerSampleMode7", };
    public static readonly string[] LayerBlendModeParameters = new[] { "_LayerBlendMode0", "_LayerBlendMode1", "_LayerBlendMode2", "_LayerBlendMode3", "_LayerBlendMode4", "_LayerBlendMode5", "_LayerBlendMode6", "_LayerBlendMode7", };
    public static readonly string[] LayerMaskTypeParameters = new[] { "_LayerMaskType0", "_LayerMaskType1", "_LayerMaskType2", "_LayerMaskType3", "_LayerMaskType4", "_LayerMaskType5", "_LayerMaskType6", "_LayerMaskType7", };
    public static readonly string[] LayerColorParameters = new[] { "_LayerColor0", "_LayerColor1", "_LayerColor2", "_LayerColor3", "_LayerColor4", "_LayerColor5", "_LayerColor6", "_LayerColor7", };
    public static readonly string[] LayerSurfaceParameters = new[] { "_LayerSurface0", "_LayerSurface1", "_LayerSurface2", "_LayerSurface3", "_LayerSurface4", "_LayerSurface5", "_LayerSurface6", "_LayerSurface7", };
    public static readonly string[] LayerSampleParametersParameters = new[] { "_LayerSampleParameters0", "_LayerSampleParameters1", "_LayerSampleParameters2", "_LayerSampleParameters3", "_LayerSampleParameters4", "_LayerSampleParameters5", "_LayerSampleParameters6", "_LayerSampleParameters7", };
    public static readonly string[] LayerMaskParametersParameters = new[] { "_LayerMaskParameters0", "_LayerMaskParameters1", "_LayerMaskParameters2", "_LayerMaskParameters3", "_LayerMaskParameters4", "_LayerMaskParameters5", "_LayerMaskParameters6", "_LayerMaskParameters7", };
    public static readonly string[] LayerMaskAxisParameters = new[] { "_LayerMaskAxis0", "_LayerMaskAxis1", "_LayerMaskAxis2", "_LayerMaskAxis3", "_LayerMaskAxis4", "_LayerMaskAxis5", "_LayerMaskAxis6", "_LayerMaskAxis7", };

    public SkinnedMeshRenderer RootMeshComponent;

    private Dictionary<Material, ovrAvatarMaterialState> materialStates = new Dictionary<Material, ovrAvatarMaterialState>();
    public List<OvrAvatarRenderComponent> RenderParts = new List<OvrAvatarRenderComponent>();

    private bool DrawSkeleton = false;
    private bool IsVertexDataUpdating = false;
    private bool IsCombiningMeshes = false;
    private bool FirstMaterialUpdate = true;

    private ulong ClothingAlphaTexture = 0;
    private Vector4 ClothingAlphaOffset;


    // We need to copy the mesh data to manipulate it in another thread.
    private struct MeshThreadData
    {
        public Color[] MeshColors;
        public int VertexCount;
        public bool IsDarkMaterial;
        public bool UsesAlpha;
    }

    Thread VertexThread;
    MeshThreadData[] ThreadData;

    public void StartMeshCombining(ovrAvatarComponent component)
    {
        IsCombiningMeshes = true;
        gameObject.SetActive(false);

        ThreadData = new MeshThreadData[RenderParts.Count];
        const UInt32 BODY_INDEX = 0;

        for (UInt32 renderPartIndex = 0; renderPartIndex < RenderParts.Count; renderPartIndex++)
        {
            var renderPart = RenderParts[(int)renderPartIndex];
            IntPtr ovrRenderPart = OvrAvatar.GetRenderPart(component, renderPartIndex);
            var materialState = CAPI.ovrAvatarSkinnedMeshRender_GetMaterialState(ovrRenderPart);

            ThreadData[renderPartIndex].VertexCount = renderPart.mesh.sharedMesh.vertexCount;
            ThreadData[renderPartIndex].IsDarkMaterial = renderPartIndex != 0;

            if (materialState.alphaMaskTextureID != 0)
            {
                if (renderPartIndex != BODY_INDEX)
                {
                    ClothingAlphaOffset = materialState.alphaMaskScaleOffset;
                    ClothingAlphaTexture = materialState.alphaMaskTextureID;
                }

                ThreadData[renderPartIndex].UsesAlpha = true;
            }

            ThreadData[renderPartIndex].MeshColors = renderPart.mesh.sharedMesh.colors;
        }

        VertexThread = new Thread(() => UpdateVertices(ref ThreadData));
        VertexThread.Start();
    }

    private void UpdateVertices(ref MeshThreadData[] threadData)
    {
        IsVertexDataUpdating = true;

        foreach (var data in threadData)
        {
            for (int index = 0; index < data.VertexCount; index++)
            {
                data.MeshColors[index].a = 0.0f;

                if (data.UsesAlpha)
                {
                    data.MeshColors[index].a = data.IsDarkMaterial ? 1.0f : 0.5f;
                }

                data.MeshColors[index].r = data.IsDarkMaterial ? 1.0f : 0.0f;
            }
        }

        IsVertexDataUpdating = false;
    }

    public void CombineMeshes(ovrAvatarComponent component)
    {
        List<Transform> bones = new List<Transform>();
        List<BoneWeight> boneWeights = new List<BoneWeight>();
        List<CombineInstance> combineInstances = new List<CombineInstance>();
        List<Matrix4x4> bindposes = new List<Matrix4x4>();
        List<Color> colors = new List<Color>();

        RootMeshComponent = gameObject.AddComponent<SkinnedMeshRenderer>();
        RootMeshComponent.quality = SkinQuality.Bone4;
        RootMeshComponent.updateWhenOffscreen = true;

        int boneOffset = 0;

        for (UInt32 renderPartIndex = 0; renderPartIndex < RenderParts.Count; renderPartIndex++)
        {
            var renderPart = RenderParts[(int)renderPartIndex];

            if (RootMeshComponent.sharedMaterial == null)
            {
                RootMeshComponent.sharedMaterial = renderPart.mesh.sharedMaterial;
                materialStates.Add(RootMeshComponent.sharedMaterial, new ovrAvatarMaterialState());

                RootMeshComponent.sortingLayerID = renderPart.mesh.sortingLayerID;
                RootMeshComponent.gameObject.layer = renderPart.gameObject.layer;
            }

            colors.AddRange(ThreadData[renderPartIndex].MeshColors);

            foreach (BoneWeight bw in renderPart.mesh.sharedMesh.boneWeights)
            {
                BoneWeight bWeight = bw;

                bWeight.boneIndex0 += boneOffset;
                bWeight.boneIndex1 += boneOffset;
                bWeight.boneIndex2 += boneOffset;
                bWeight.boneIndex3 += boneOffset;

                boneWeights.Add(bWeight);
            }

            boneOffset += renderPart.mesh.bones.Length;

            foreach (Transform bone in renderPart.mesh.bones)
            {
                bones.Add(bone);
            }

            CombineInstance ci = new CombineInstance();
            ci.mesh = renderPart.mesh.sharedMesh;
            ci.transform = renderPart.mesh.transform.localToWorldMatrix;
            combineInstances.Add(ci);

            for (int b = 0; b < renderPart.mesh.bones.Length; b++)
            {
                bindposes.Add(renderPart.mesh.sharedMesh.bindposes[b]);
            }

            Destroy(renderPart.mesh);
            renderPart.mesh = null;
        }

        RootMeshComponent.sharedMesh = new Mesh();
        RootMeshComponent.sharedMesh.name = transform.name + "_combined_mesh";
        RootMeshComponent.sharedMesh.CombineMeshes(combineInstances.ToArray(), true, true);

        RootMeshComponent.bones = bones.ToArray();
        RootMeshComponent.sharedMesh.boneWeights = boneWeights.ToArray();
        RootMeshComponent.sharedMesh.bindposes = bindposes.ToArray();
        RootMeshComponent.sharedMesh.colors = colors.ToArray();
        RootMeshComponent.rootBone = bones[0];
        RootMeshComponent.sharedMesh.RecalculateBounds();
    }

    public void UpdateAvatar(ovrAvatarComponent component, OvrAvatar avatar)
    {
        if (IsCombiningMeshes)
        {
            if (!IsVertexDataUpdating)
            {
                CombineMeshes(component);
                IsCombiningMeshes = false;
                ThreadData = null;
            }

            return;
        }

        OvrAvatar.ConvertTransform(component.transform, transform);

        for (UInt32 renderPartIndex = 0; renderPartIndex < component.renderPartCount; renderPartIndex++)
        {
            if (RenderParts.Count <= renderPartIndex)
            {
                break;
            }

            OvrAvatarRenderComponent renderComponent = RenderParts[(int)renderPartIndex];
            IntPtr renderPart = OvrAvatar.GetRenderPart(component, renderPartIndex);
            ovrAvatarRenderPartType type = CAPI.ovrAvatarRenderPart_GetType(renderPart);
            switch (type)
            {
                case ovrAvatarRenderPartType.SkinnedMeshRender:
                    ((OvrAvatarSkinnedMeshRenderComponent)renderComponent).UpdateSkinnedMeshRender(this, avatar, renderPart);
                    break;
                case ovrAvatarRenderPartType.SkinnedMeshRenderPBS:
                    var material = RootMeshComponent != null ? RootMeshComponent.sharedMaterial : renderComponent.mesh.sharedMaterial;
                    ((OvrAvatarSkinnedMeshRenderPBSComponent)renderComponent).UpdateSkinnedMeshRenderPBS(avatar, renderPart, material);
                    break;
                case ovrAvatarRenderPartType.ProjectorRender:
                    ((OvrAvatarProjectorRenderComponent)renderComponent).UpdateProjectorRender(this, CAPI.ovrAvatarRenderPart_GetProjectorRender(renderPart));
                    break;
                default:
                    break;
            }
        }

        // The mesh has been combined and therefore update the combined mesh "components"
        if (RootMeshComponent != null)
        {
            var part_zero = OvrAvatar.GetRenderPart(component, 0);

            ovrAvatarRenderPartType typeZero = CAPI.ovrAvatarRenderPart_GetType(part_zero);
            switch (typeZero)
            {
                case ovrAvatarRenderPartType.SkinnedMeshRender:
                    UpdateActive(avatar, CAPI.ovrAvatarSkinnedMeshRender_GetVisibilityMask(part_zero));
                    bool changedMaterial = (FirstMaterialUpdate && gameObject.activeSelf) || CAPI.ovrAvatarSkinnedMeshRender_MaterialStateChanged(part_zero);
                    if (changedMaterial)
                    {
                        FirstMaterialUpdate = false;
                        ovrAvatarMaterialState materialState = CAPI.ovrAvatarSkinnedMeshRender_GetMaterialState(part_zero);
                        UpdateAvatarMaterial(RootMeshComponent.sharedMaterial, materialState);
                    }
                    break;
                case ovrAvatarRenderPartType.SkinnedMeshRenderPBS:
                    UpdateActive(avatar, CAPI.ovrAvatarSkinnedMeshRenderPBS_GetVisibilityMask(part_zero));
                    break;
                case ovrAvatarRenderPartType.ProjectorRender:
                default:
                    break;
            }
        }

        DebugDrawTransforms();
    }

    protected void UpdateActive(OvrAvatar avatar, ovrAvatarVisibilityFlags mask)
    {
        bool active = avatar.ShowFirstPerson && (mask & ovrAvatarVisibilityFlags.FirstPerson) != 0;
        active |= avatar.ShowThirdPerson && (mask & ovrAvatarVisibilityFlags.ThirdPerson) != 0;
        this.gameObject.SetActive(active);
    }

    private void DebugDrawTransforms()
    {
        Color[] line_color = { Color.red, Color.white, Color.blue };
        int color_index = 0;

        if (DrawSkeleton && RootMeshComponent != null && RootMeshComponent.bones != null)
        {
            foreach (var bone in RootMeshComponent.bones)
            {
                if (bone.parent)
                {
                    Debug.DrawLine(bone.position, bone.parent.position, line_color[color_index++ % line_color.Length]);
                }
            }
        }
    }

    public void UpdateAvatarMaterial(Material mat, ovrAvatarMaterialState matState)
    {
        mat.SetColor("_BaseColor", matState.baseColor);
        mat.SetInt("_BaseMaskType", (int)matState.baseMaskType);
        mat.SetVector("_BaseMaskParameters", matState.baseMaskParameters);
        mat.SetVector("_BaseMaskAxis", matState.baseMaskAxis);

        if (matState.alphaMaskTextureID != 0)
        {
            mat.SetTexture("_AlphaMask", GetLoadedTexture(matState.alphaMaskTextureID));
            mat.SetTextureScale("_AlphaMask", new Vector2(matState.alphaMaskScaleOffset.x, matState.alphaMaskScaleOffset.y));
            mat.SetTextureOffset("_AlphaMask", new Vector2(matState.alphaMaskScaleOffset.z, matState.alphaMaskScaleOffset.w));
        }

        if (ClothingAlphaTexture != 0)
        {
            mat.EnableKeyword("VERTALPHA_ON");
            mat.SetTexture("_AlphaMask2", GetLoadedTexture(ClothingAlphaTexture));
            mat.SetTextureScale("_AlphaMask2", new Vector2(ClothingAlphaOffset.x, ClothingAlphaOffset.y));
            mat.SetTextureOffset("_AlphaMask2", new Vector2(ClothingAlphaOffset.z, ClothingAlphaOffset.w));
        }

        if (matState.normalMapTextureID != 0)
        {
            mat.EnableKeyword("NORMAL_MAP_ON");
            mat.SetTexture("_NormalMap", GetLoadedTexture(matState.normalMapTextureID));
            mat.SetTextureScale("_NormalMap", new Vector2(matState.normalMapScaleOffset.x, matState.normalMapScaleOffset.y));
            mat.SetTextureOffset("_NormalMap", new Vector2(matState.normalMapScaleOffset.z, matState.normalMapScaleOffset.w));
        }
        if (matState.parallaxMapTextureID != 0)
        {
            mat.SetTexture("_ParallaxMap", GetLoadedTexture(matState.parallaxMapTextureID));
            mat.SetTextureScale("_ParallaxMap", new Vector2(matState.parallaxMapScaleOffset.x, matState.parallaxMapScaleOffset.y));
            mat.SetTextureOffset("_ParallaxMap", new Vector2(matState.parallaxMapScaleOffset.z, matState.parallaxMapScaleOffset.w));
        }
        if (matState.roughnessMapTextureID != 0)
        {
            mat.EnableKeyword("ROUGHNESS_ON");
            mat.SetTexture("_RoughnessMap", GetLoadedTexture(matState.roughnessMapTextureID));
            mat.SetTextureScale("_RoughnessMap", new Vector2(matState.roughnessMapScaleOffset.x, matState.roughnessMapScaleOffset.y));
            mat.SetTextureOffset("_RoughnessMap", new Vector2(matState.roughnessMapScaleOffset.z, matState.roughnessMapScaleOffset.w));
        }
        mat.EnableKeyword(LayerKeywords[matState.layerCount]);
        for (ulong layerIndex = 0; layerIndex < matState.layerCount; layerIndex++)
        {
            ovrAvatarMaterialLayerState layer = matState.layers[layerIndex];

            mat.SetInt(LayerSampleModeParameters[layerIndex], (int)layer.sampleMode);
            mat.SetInt(LayerBlendModeParameters[layerIndex], (int)layer.blendMode);
            mat.SetInt(LayerMaskTypeParameters[layerIndex], (int)layer.maskType);
            mat.SetColor(LayerColorParameters[layerIndex], layer.layerColor);
            if (layer.sampleMode != ovrAvatarMaterialLayerSampleMode.Color)
            {
                string surfaceProperty = LayerSurfaceParameters[layerIndex];
                mat.SetTexture(surfaceProperty, GetLoadedTexture(layer.sampleTexture));
                mat.SetTextureScale(surfaceProperty, new Vector2(layer.sampleScaleOffset.x, layer.sampleScaleOffset.y));
                mat.SetTextureOffset(surfaceProperty, new Vector2(layer.sampleScaleOffset.z, layer.sampleScaleOffset.w));
            }

            if (layer.sampleMode == ovrAvatarMaterialLayerSampleMode.Parallax)
            {
                mat.EnableKeyword("PARALLAX_ON");
            }

            mat.SetColor(LayerSampleParametersParameters[layerIndex], layer.sampleParameters);
            mat.SetColor(LayerMaskParametersParameters[layerIndex], layer.maskParameters);
            mat.SetColor(LayerMaskAxisParameters[layerIndex], layer.maskAxis);
        }

        materialStates[mat] = matState;
    }

    public static Texture2D GetLoadedTexture(UInt64 assetId)
    {
        if (assetId == 0)
        {
            return null;
        }
        OvrAvatarAssetTexture tex = (OvrAvatarAssetTexture)OvrAvatarSDKManager.Instance.GetAsset(assetId);
        if (tex == null)
        {
            throw new Exception("Could not find texture for asset " + assetId);
        }
        return tex.texture;
    }
}
