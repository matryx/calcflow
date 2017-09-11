using System;
using Oculus.Avatar;
using UnityEngine;

public class OvrAvatarAssetTexture : OvrAvatarAsset {
    public Texture2D texture;

    private const int ASTCHeaderSize = 16;

    public OvrAvatarAssetTexture(UInt64 _assetId, IntPtr asset) {
        assetID = _assetId;
        ovrAvatarTextureAssetData textureAssetData = CAPI.ovrAvatarAsset_GetTextureData(asset);
        TextureFormat format;
        IntPtr textureData = textureAssetData.textureData;
        int textureDataSize = (int)textureAssetData.textureDataSize;
        switch (textureAssetData.format)
        {
            case ovrAvatarTextureFormat.RGB24:
                format = TextureFormat.RGB24;
                break;
            case ovrAvatarTextureFormat.DXT1:
                format = TextureFormat.DXT1;
                break;
            case ovrAvatarTextureFormat.DXT5:
                format = TextureFormat.DXT5;
                break;
            case ovrAvatarTextureFormat.ASTC_RGB_6x6:
                format = TextureFormat.ASTC_RGB_6x6;
                textureData = new IntPtr(textureData.ToInt64() + ASTCHeaderSize);
                textureDataSize -= ASTCHeaderSize;
                break;
            default:
                throw new NotImplementedException(
                    string.Format("Unsupported texture format {0}",
                                  textureAssetData.format.ToString()));
        }
        texture = new Texture2D(
            (int)textureAssetData.sizeX, (int)textureAssetData.sizeY,
            format, textureAssetData.mipCount > 1, false);
        texture.LoadRawTextureData(textureData, textureDataSize);
        texture.Apply(true, false);
    }
}
