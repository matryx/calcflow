using UnityEngine;
using System;
using Oculus.Avatar;
using System.Collections.Generic;

public class OvrAvatarMeshInstance : MonoBehaviour
{
    HashSet<UInt64> AssetsToLoad;

    public UInt64 MeshID = 0;
    UInt64 MaterialID = 0;
    UInt64 FadeTextureID = 0;
    public ovrAvatarBodyPartType MeshType;
    public ovrAvatarMaterialState MaterialState;
    MeshFilter Mesh;
    MeshRenderer MeshInstance;

    public void AssetLoadedCallback(OvrAvatarAsset asset)
    {
        AssetsToLoad.Remove(asset.assetID);
        HandleAssetAvailable(asset);

        if (AssetsToLoad.Count <= 0)
        {
            UpdateMaterial();
        }
    }

    public void SetMeshAssets(UInt64 fadeTexture, UInt64 meshID, UInt64 materialID, ovrAvatarBodyPartType type)
    {
        MaterialID = materialID;
        MeshID = meshID;
        FadeTextureID = fadeTexture;
        MeshType = type;

        AssetsToLoad = new HashSet<UInt64>();

        RequestAsset(meshID);
        RequestAsset(materialID);
        RequestAsset(fadeTexture);
    }

    private void HandleAssetAvailable(OvrAvatarAsset asset)
    {
        if (asset.assetID == MeshID)
        {
            Mesh = gameObject.AddComponent<MeshFilter>();
            MeshInstance = gameObject.AddComponent<MeshRenderer>();
            MeshInstance.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
            Mesh.sharedMesh = ((OvrAvatarAssetMesh)asset).mesh;

            Material mat = new Material(Shader.Find("OvrAvatar/AvatarSurfaceShaderSelfOccluding"));
            MeshInstance.material = mat;
        }

        if (asset.assetID == MaterialID)
        {
            MaterialState = ((OvrAvatarAssetMaterial)asset).material;
            MaterialState.alphaMaskTextureID = FadeTextureID;
            RequestMaterialTextures();
        }
    }

    public void ChangeMaterial(UInt64 assetID)
    {
        MaterialID = assetID;
        RequestAsset(MaterialID);
    }

    private void RequestAsset(UInt64 assetID)
    {
        if (assetID == 0)
        {
            return;
        }

        OvrAvatarAsset asset = OvrAvatarSDKManager.Instance.GetAsset(assetID);
        if (asset == null)
        {
            OvrAvatarSDKManager.Instance.BeginLoadingAsset(assetID, this.AssetLoadedCallback);
            AssetsToLoad.Add(assetID);
        }
        else
        {
            HandleAssetAvailable(asset);
        }
    }

    private void RequestMaterialTextures()
    {
        RequestAsset(MaterialState.normalMapTextureID);
        RequestAsset(MaterialState.parallaxMapTextureID);
        RequestAsset(MaterialState.roughnessMapTextureID);

        for (var layerIndex = 0; layerIndex < MaterialState.layerCount; layerIndex++)
        {
            RequestAsset(MaterialState.layers[layerIndex].sampleTexture);
        }
    }

    public void SetActive(bool active)
    {
        gameObject.SetActive(active);

        if (active)
        {
            UpdateMaterial();
        }
    }

    private void UpdateMaterial()
    {
        if (MeshInstance == null || MaterialID == 0)
        {
            return;
        }

        var mat = MeshInstance.material;
        var matState = MaterialState;

        mat.SetColor("_BaseColor", matState.baseColor);
        mat.SetInt("_BaseMaskType", (int)matState.baseMaskType);
        mat.SetVector("_BaseMaskParameters", matState.baseMaskParameters);
        mat.SetVector("_BaseMaskAxis", matState.baseMaskAxis);

        if (matState.alphaMaskTextureID != 0)
        {
            mat.SetTexture("_AlphaMask", OvrAvatarComponent.GetLoadedTexture(matState.alphaMaskTextureID));
            mat.SetTextureScale("_AlphaMask", new Vector2(matState.alphaMaskScaleOffset.x, matState.alphaMaskScaleOffset.y));
            mat.SetTextureOffset("_AlphaMask", new Vector2(matState.alphaMaskScaleOffset.z, matState.alphaMaskScaleOffset.w));
        }

        if (matState.normalMapTextureID != 0)
        {
            mat.EnableKeyword("NORMAL_MAP_ON");
            mat.SetTexture("_NormalMap", OvrAvatarComponent.GetLoadedTexture(matState.normalMapTextureID));
            mat.SetTextureScale("_NormalMap", new Vector2(matState.normalMapScaleOffset.x, matState.normalMapScaleOffset.y));
            mat.SetTextureOffset("_NormalMap", new Vector2(matState.normalMapScaleOffset.z, matState.normalMapScaleOffset.w));
        }
        if (matState.parallaxMapTextureID != 0)
        {
            mat.SetTexture("_ParallaxMap", OvrAvatarComponent.GetLoadedTexture(matState.parallaxMapTextureID));
            mat.SetTextureScale("_ParallaxMap", new Vector2(matState.parallaxMapScaleOffset.x, matState.parallaxMapScaleOffset.y));
            mat.SetTextureOffset("_ParallaxMap", new Vector2(matState.parallaxMapScaleOffset.z, matState.parallaxMapScaleOffset.w));
        }
        if (matState.roughnessMapTextureID != 0)
        {
            mat.EnableKeyword("ROUGHNESS_ON");
            mat.SetTexture("_RoughnessMap", OvrAvatarComponent.GetLoadedTexture(matState.roughnessMapTextureID));
            mat.SetTextureScale("_RoughnessMap", new Vector2(matState.roughnessMapScaleOffset.x, matState.roughnessMapScaleOffset.y));
            mat.SetTextureOffset("_RoughnessMap", new Vector2(matState.roughnessMapScaleOffset.z, matState.roughnessMapScaleOffset.w));
        }
        mat.EnableKeyword(OvrAvatarComponent.LayerKeywords[matState.layerCount]);
        for (ulong layerIndex = 0; layerIndex < matState.layerCount; layerIndex++)
        {
            ovrAvatarMaterialLayerState layer = matState.layers[layerIndex];

            mat.SetInt(OvrAvatarComponent.LayerSampleModeParameters[layerIndex], (int)layer.sampleMode);
            mat.SetInt(OvrAvatarComponent.LayerBlendModeParameters[layerIndex], (int)layer.blendMode);
            mat.SetInt(OvrAvatarComponent.LayerMaskTypeParameters[layerIndex], (int)layer.maskType);
            mat.SetColor(OvrAvatarComponent.LayerColorParameters[layerIndex], layer.layerColor);
            if (layer.sampleMode != ovrAvatarMaterialLayerSampleMode.Color)
            {
                string surfaceProperty = OvrAvatarComponent.LayerSurfaceParameters[layerIndex];
                mat.SetTexture(surfaceProperty, OvrAvatarComponent.GetLoadedTexture(layer.sampleTexture));
                mat.SetTextureScale(surfaceProperty, new Vector2(layer.sampleScaleOffset.x, layer.sampleScaleOffset.y));
                mat.SetTextureOffset(surfaceProperty, new Vector2(layer.sampleScaleOffset.z, layer.sampleScaleOffset.w));
            }

            if (layer.sampleMode == ovrAvatarMaterialLayerSampleMode.Parallax)
            {
                mat.EnableKeyword("PARALLAX_ON");
            }

            mat.SetColor(OvrAvatarComponent.LayerSampleParametersParameters[layerIndex], layer.sampleParameters);
            mat.SetColor(OvrAvatarComponent.LayerMaskParametersParameters[layerIndex], layer.maskParameters);
            mat.SetColor(OvrAvatarComponent.LayerMaskAxisParameters[layerIndex], layer.maskAxis);
        }
    }
}
