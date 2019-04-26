using UnityEngine;
using System.Collections;
using System;
using System.Runtime.InteropServices;
using Oculus.Avatar;

//This needs to be the csharp equivalent of ovrAvatarCapabilities in OVR_Avatar.h
[Flags]
public enum ovrAvatarCapabilities
{
    Body = 1 << 0,
    Hands = 1 << 1,
    Base = 1 << 2,
    BodyTilt = 1 << 4,
    All = -1
};

// This needs to be the csharp equivalent of ovrAvatarMessageType in OVR_Avatar.h
public enum ovrAvatarMessageType {
    AvatarSpecification,
    AssetLoaded,
    Count
};

// This needs to be the csharp equivalent of ovrAvatarMessage_AvatarSpecification in OVR_Avatar.h
public struct ovrAvatarMessage_AvatarSpecification {
    public IntPtr avatarSpec; //ovrAvatarSpecification*, opaque pointer
    public UInt64 oculusUserID;
};

// This needs to be the csharp equivalent of ovrAvatarMessage_AssetLoaded in OVR_Avatar.h
public struct ovrAvatarMessage_AssetLoaded {
    public UInt64 assetID;
    public IntPtr asset; //ovrAvatarAsset*, opaque pointer
};

// This needs to be the csharp equivalent of ovrAvatarAssetType in OVR_Avatar.h
public enum ovrAvatarAssetType {
    Mesh,
    Texture,
    Pose,
    Material,
    Count
};

// This needs to be the csharp equivalent of ovrAvatarMeshVertex in OVR_Avatar.h
public struct ovrAvatarMeshVertex
{
    public float x;
    public float y;
    public float z;
    public float nx;
    public float ny;
    public float nz;
	public float tx;
	public float ty;
	public float tz;
	public float tw;
    public float u;
    public float v;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public byte[] blendIndices;     ///< Indices into the bind pose

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public float[] blendWeights;     ///< Blend weights for each component in the bind pose
};

// This needs to be the csharp equivalent of ovrAvatarMeshAssetData in OVR_Avatar.h
public struct ovrAvatarMeshAssetData {
	public UInt32                   vertexCount;
	public IntPtr                   vertexBuffer; //const ovrAvatarMeshVertex*
	public UInt32                   indexCount;
	public IntPtr                   indexBuffer; //const uint16t*
	public ovrAvatarSkinnedMeshPose skinnedBindPose;
};

// This needs to be the csharp equivalent of ovrAvatarTextureFormat in OVR_Avatar.h
public enum ovrAvatarTextureFormat {
	RGB24,
	DXT1,
	DXT5,
	ASTC_RGB_6x6,
	Count
};

// This needs to be the csharp equivalent of ovrAvatarTextureAssetData in OVR_Avatar.h
public struct ovrAvatarTextureAssetData {
	public ovrAvatarTextureFormat format;
	public UInt32 sizeX;
	public UInt32 sizeY;
	public UInt32 mipCount;
	public UInt64 textureDataSize;
	public IntPtr textureData; // const uint8_t*	
};

// This needs to be the csharp equivalent of ovrAvatarRenderPartType in OVR_Avatar.h
public enum ovrAvatarRenderPartType
{
    SkinnedMeshRender,
    SkinnedMeshRenderPBS,
    ProjectorRender,
    Count
};

// This needs to be the csharp equivalent of ovrAvatarTransform in OVR_Avatar.h
public struct ovrAvatarTransform
{
    public Vector3 position;
    public Quaternion orientation;
    public Vector3 scale;
};

// This needs to be the csharp equivalent of ovrAvatarButton in OVR_Avatar.h
[Flags]
public enum ovrAvatarButton
{
    One = 0x0001,
    Two = 0x0002,
    Three = 0x0004,
    Joystick = 0x0008,
}

// This needs to be the csharp equivalent of ovrAvatarTouch in OVR_Avatar.h
[Flags]
public enum ovrAvatarTouch
{
    One = 0x0001,
    Two = 0x0002,
    Joystick = 0x0004,
    ThumbRest = 0x0008,
    Index = 0x0010,
    Pointing = 0x0040,
    ThumbUp = 0x0080,
}

// This needs to be the csharp equivalent of ovrAvatarHandInputState in OVR_Avatar.h
public struct ovrAvatarHandInputState
{
    public ovrAvatarTransform transform;
	public ovrAvatarButton buttonMask;
	public ovrAvatarTouch touchMask;
	public float joystickX;
	public float joystickY;
	public float indexTrigger;
	public float handTrigger;
    [MarshalAs(UnmanagedType.I1)]
    public bool isActive;
};

// This needs to be the csharp equivalent of ovrAvatarComponent in OVR_Avatar.h
public struct ovrAvatarComponent
{
    public ovrAvatarTransform transform;
    public UInt32 renderPartCount;
    public IntPtr renderParts; //const ovrAvatarRenderPart* const*

    [MarshalAs(UnmanagedType.LPStr)]
    public string name;
};

// This needs to be the csharp equivalent of ovrAvatarBodyComponent in OVR_Avatar.h
public struct ovrAvatarBaseComponent
{
    public Vector3 basePosition;
    public IntPtr renderComponent; //const ovrAvatarComponent*
};

// This needs to be the csharp equivalent of ovrAvatarBodyComponent in OVR_Avatar.h
public struct ovrAvatarBodyComponent {
	public ovrAvatarTransform  leftEyeTransform;
	public ovrAvatarTransform  rightEyeTransform;
	public ovrAvatarTransform  centerEyeTransform;
	public IntPtr              renderComponent; //const ovrAvatarComponent*
};

// This needs to be the csharp equivalent of ovrAvatarControllerComponent in OVR_Avatar.h
public struct ovrAvatarControllerComponent
{
    public ovrAvatarHandInputState inputState;
    public IntPtr renderComponent; //const ovrAvatarComponent*
};

// This needs to be the csharp equivalent of ovrAvatarHandComponent in OVR_Avatar.h
public struct ovrAvatarHandComponent {
    public ovrAvatarHandInputState inputState;
    public IntPtr renderComponent; //const ovrAvatarComponent*
};

// This needs to be the csharp equivalent of ovrAvatarMaterialLayerBlendMode in OVR_Avatar.h
public enum ovrAvatarMaterialLayerBlendMode{
	Add,
	Multiply,
	Count
};

// This needs to be the csharp equivalent of ovrAvatarMaterialLayerSampleMode in OVR_Avatar.h
public enum ovrAvatarMaterialLayerSampleMode{
	Color,
	Texture,
	TextureSingleChannel,
	Parallax,
	Count
};

// This needs to be the csharp equivalent of ovrAvatarMaterialLayerMaskType in OVR_Avatar.h
public enum ovrAvatarMaterialMaskType{
	None,
	Positional,
	ViewReflection,
	Fresnel,
    Pulse,
	Count
};

// This needs to be the csharp equivalent of ovrAvatarMaterialLayerState in OVR_Avatar.h
public struct ovrAvatarMaterialLayerState{
	public ovrAvatarMaterialLayerBlendMode  blendMode;
	public ovrAvatarMaterialLayerSampleMode sampleMode;
	public ovrAvatarMaterialMaskType        maskType;
	public Vector4                          layerColor;
	public Vector4                          sampleParameters;
	public UInt64                           sampleTexture;
    public Vector4                          sampleScaleOffset;
	public Vector4                          maskParameters;
	public Vector4                          maskAxis;

    static bool VectorEquals(Vector4 a, Vector4 b)
    {
        return a.x == b.x && a.y == b.y && a.z == b.z && a.w == b.w;
    }

    public override bool Equals(object obj)
    {
        if (!(obj is ovrAvatarMaterialLayerState))
        {
            return false;
        }
        ovrAvatarMaterialLayerState other = (ovrAvatarMaterialLayerState)obj;
        if (blendMode != other.blendMode) return false;
        if (sampleMode != other.sampleMode) return false;
        if (maskType != other.maskType) return false;
        if (!VectorEquals(layerColor, other.layerColor)) return false;
        if (!VectorEquals(sampleParameters, other.sampleParameters)) return false;
        if (sampleTexture != other.sampleTexture) return false;
        if (!VectorEquals(sampleScaleOffset, other.sampleScaleOffset)) return false;
        if (!VectorEquals(maskParameters, other.maskParameters)) return false;
        if (!VectorEquals(maskAxis, other.maskAxis)) return false;
        return true;
    }
    public override int GetHashCode()
    {
        return blendMode.GetHashCode() ^
            sampleMode.GetHashCode() ^
            maskType.GetHashCode() ^
            layerColor.GetHashCode() ^
            sampleParameters.GetHashCode() ^
            sampleTexture.GetHashCode() ^
            sampleScaleOffset.GetHashCode() ^
            maskParameters.GetHashCode() ^
            maskAxis.GetHashCode();
    }
};

// This needs to be the csharp equivalent of ovrAvatarMaterialState in OVR_Avatar.h
public struct ovrAvatarMaterialState{
	public Vector4 baseColor;
    public ovrAvatarMaterialMaskType baseMaskType;
    public Vector4 baseMaskParameters;
    public Vector4 baseMaskAxis;
    public ovrAvatarMaterialLayerSampleMode sampleMode;
	public UInt64 alphaMaskTextureID;
    public Vector4 alphaMaskScaleOffset;
	public UInt64 normalMapTextureID;
    public Vector4 normalMapScaleOffset;
    public UInt64 parallaxMapTextureID;
    public Vector4 parallaxMapScaleOffset;
	public UInt64 roughnessMapTextureID;
    public Vector4 roughnessMapScaleOffset;
	public UInt32 layerCount;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 8)]
	public ovrAvatarMaterialLayerState[] layers;

    static bool VectorEquals(Vector4 a, Vector4 b)
    {
        return a.x == b.x && a.y == b.y && a.z == b.z && a.w == b.w;
    }

    public override bool Equals(object obj)
    {
        if (!(obj is ovrAvatarMaterialState))
        {
            return false;
        }
        ovrAvatarMaterialState other = (ovrAvatarMaterialState)obj;
        if (!VectorEquals(baseColor, other.baseColor)) return false;
        if (baseMaskType != other.baseMaskType) return false;
        if (!VectorEquals(baseMaskParameters, other.baseMaskParameters)) return false;
        if (!VectorEquals(baseMaskAxis, other.baseMaskAxis)) return false;
        if (sampleMode != other.sampleMode) return false;
        if (alphaMaskTextureID != other.alphaMaskTextureID) return false;
        if (!VectorEquals(alphaMaskScaleOffset, other.alphaMaskScaleOffset)) return false;
        if (normalMapTextureID != other.normalMapTextureID) return false;
        if (!VectorEquals(normalMapScaleOffset, other.normalMapScaleOffset)) return false;
        if (parallaxMapTextureID != other.parallaxMapTextureID) return false;
        if (!VectorEquals(parallaxMapScaleOffset, other.parallaxMapScaleOffset)) return false;
        if (roughnessMapTextureID != other.roughnessMapTextureID) return false;
        if (!VectorEquals(roughnessMapScaleOffset, other.roughnessMapScaleOffset)) return false;
        if (layerCount != other.layerCount) return false;
        for (int i = 0; i < layerCount; ++i)
        {
            if (!layers[i].Equals(other.layers[i])) return false;
        }
        return true;
    }

    public override int GetHashCode()
    {
        int hash = 0;
        hash ^= baseColor.GetHashCode();
        hash ^= baseMaskType.GetHashCode();
        hash ^= baseMaskParameters.GetHashCode();
        hash ^= baseMaskAxis.GetHashCode();
        hash ^= sampleMode.GetHashCode();
        hash ^= alphaMaskTextureID.GetHashCode();
        hash ^= alphaMaskScaleOffset.GetHashCode();
        hash ^= normalMapTextureID.GetHashCode();
        hash ^= normalMapScaleOffset.GetHashCode();
        hash ^= parallaxMapTextureID.GetHashCode();
        hash ^= parallaxMapScaleOffset.GetHashCode();
        hash ^= roughnessMapTextureID.GetHashCode();
        hash ^= roughnessMapScaleOffset.GetHashCode();
        hash ^= layerCount.GetHashCode();
        for (int i = 0; i < layerCount; ++i)
        {
            hash ^= layers[i].GetHashCode();
        }
        return hash;
    }
};

public class OvrAvatarAssetMaterial : OvrAvatarAsset
{
    public OvrAvatarAssetMaterial(UInt64 id, IntPtr mat) 
    {
        assetID = id;
        material = CAPI.ovrAvatarAsset_GetMaterialState(mat);
    }

    public ovrAvatarMaterialState material;
}
// This needs to be the csharp equivalent of ovrAvatarSkinnedMeshPose in OVR_Avatar.h
public struct ovrAvatarSkinnedMeshPose
{
    public UInt32 jointCount;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 64)]
    public ovrAvatarTransform[] jointTransform;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 64)]
    public int[] jointParents;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 64)]
    public IntPtr[] jointNames; //const char * jointNames[64];
};


[Flags]
public enum ovrAvatarVisibilityFlags
{
    FirstPerson = 1 << 0,
    ThirdPerson = 1 << 1,
    SelfOccluding = 1 << 2,
};

// This needs to be the csharp equivalent of ovrAvatarRenderPart_SkinnedMeshRender in OVR_Avatar.h
public struct ovrAvatarRenderPart_SkinnedMeshRender
{
    public ovrAvatarTransform localTransform;
    public ovrAvatarVisibilityFlags visibilityMask;
    public UInt64 meshAssetID;
    public ovrAvatarMaterialState materialState;
	public ovrAvatarSkinnedMeshPose skinnedPose;
};

// This needs to be the csharp equivalent of ovrAvatarRenderPart_SkinnedMeshRenderPBS in OVR_Avatar.h
public struct ovrAvatarRenderPart_SkinnedMeshRenderPBS
{
    public ovrAvatarTransform localTransform;
    public ovrAvatarVisibilityFlags visibilityMask;
    public UInt64 meshAssetID;
    public UInt64 albedoTextureAssetID;
    public UInt64 surfaceTextureAssetID;
    public ovrAvatarSkinnedMeshPose skinnedPose;
};

// This needs to be the csharp equivalent of ovrAvatarRenderPart_ProjectorRender in OVR_Avatar.h
public struct ovrAvatarRenderPart_ProjectorRender
{
    public ovrAvatarTransform localTransform;
    public UInt32 componentIndex;
    public UInt32 renderPartIndex;
    public ovrAvatarMaterialState materialState;
};

// This needs to be the csharp equivalent of ovrAvatarHandGesture in OVR_Avatar.h
public enum ovrAvatarHandGesture {
    Default,
    GripSphere,
    GripCube,
    Count
};

public enum ovrAvatarBodyPartType
{
    Body,
    Clothing,
    Eyewear,
    Hair,
    Beard
};

namespace Oculus.Avatar
{
    public class CAPI
    {
#if UNITY_ANDROID && !UNITY_EDITOR
        private const string LibFile = "ovravatarloader";

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatar_InitializeAndroidUnity(string appID);
#else
        private const string LibFile = "libovravatar";

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatar_Initialize(string appID);
#endif

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatar_Shutdown();

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ovrAvatarMessage_Pop();

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern ovrAvatarMessageType ovrAvatarMessage_GetType(IntPtr msg);

        public static ovrAvatarMessage_AvatarSpecification ovrAvatarMessage_GetAvatarSpecification(
            IntPtr msg)
        {
            IntPtr ptr = ovrAvatarMessage_GetAvatarSpecification_Native(msg);
            return (ovrAvatarMessage_AvatarSpecification)Marshal.PtrToStructure(
                ptr, typeof(ovrAvatarMessage_AvatarSpecification));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarMessage_GetAvatarSpecification")]
        private static extern IntPtr ovrAvatarMessage_GetAvatarSpecification_Native(IntPtr msg);

        public static ovrAvatarMessage_AssetLoaded ovrAvatarMessage_GetAssetLoaded(
            IntPtr msg)
        {
            IntPtr ptr = ovrAvatarMessage_GetAssetLoaded_Native(msg);
            return (ovrAvatarMessage_AssetLoaded)Marshal.PtrToStructure(
                ptr, typeof(ovrAvatarMessage_AssetLoaded));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarMessage_GetAssetLoaded")]
        private static extern IntPtr ovrAvatarMessage_GetAssetLoaded_Native(IntPtr msg);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatarMessage_Free(IntPtr msg);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatar_RequestAvatarSpecification(UInt64 userID);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ovrAvatar_Create(IntPtr avatarSpecification,
            ovrAvatarCapabilities capabilities);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatar_Destroy(IntPtr avatar);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatarPose_UpdateBody(
            IntPtr avatar, ovrAvatarTransform headPose);

        public static void ovrAvatarPose_UpdateVoiceVisualization(
            IntPtr avatar, float[] pcmData)
        {
            ovrAvatarPose_UpdateVoiceVisualization_Native(
                avatar, (UInt32)pcmData.Length, pcmData);
        }
        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarPose_UpdateVoiceVisualization")]
        private static extern void ovrAvatarPose_UpdateVoiceVisualization_Native(
            IntPtr avatar, UInt32 pcmDataSize, [In] float[] pcmData);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatarPose_UpdateHands(
            IntPtr avatar,
	        ovrAvatarHandInputState inputStateLeft,
            ovrAvatarHandInputState inputStateRight);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatarPose_Finalize(IntPtr avatar, float elapsedSeconds);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatar_SetLeftControllerVisibility(IntPtr avatar, bool show);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatar_SetRightControllerVisibility(IntPtr avatar, bool show);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern UInt32 ovrAvatarComponent_Count(IntPtr avatar);

        public static ovrAvatarComponent ovrAvatarComponent_Get(
            IntPtr avatar, UInt32 index)
        {
            IntPtr ptr = ovrAvatarComponent_Get_Native(avatar, index);
            return (ovrAvatarComponent)Marshal.PtrToStructure(
                ptr, typeof(ovrAvatarComponent));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarComponent_Get")]
        public static extern IntPtr ovrAvatarComponent_Get_Native(IntPtr avatar, UInt32 index);

        public static ovrAvatarBaseComponent? ovrAvatarPose_GetBaseComponent(
            IntPtr avatar)
        {
            IntPtr ptr = ovrAvatarPose_GetBaseComponent_Native(avatar);
            return ptr == IntPtr.Zero
                ?  (ovrAvatarBaseComponent?)null
                :  (ovrAvatarBaseComponent)Marshal.PtrToStructure(ptr, typeof(ovrAvatarBaseComponent));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarPose_GetBaseComponent")]
        private static extern IntPtr ovrAvatarPose_GetBaseComponent_Native(IntPtr avatar);

        public static ovrAvatarBodyComponent? ovrAvatarPose_GetBodyComponent(
            IntPtr avatar)
        {
            IntPtr ptr = ovrAvatarPose_GetBodyComponent_Native(avatar);
            return ptr == IntPtr.Zero
                ?  (ovrAvatarBodyComponent?)null
                :  (ovrAvatarBodyComponent)Marshal.PtrToStructure(ptr, typeof(ovrAvatarBodyComponent));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarPose_GetBodyComponent")]
        private static extern IntPtr ovrAvatarPose_GetBodyComponent_Native(IntPtr avatar);

        public static ovrAvatarControllerComponent? ovrAvatarPose_GetLeftControllerComponent(
    IntPtr avatar)
        {
            IntPtr ptr = ovrAvatarPose_GetLeftControllerComponent_Native(avatar);
            return ptr == IntPtr.Zero
                ?  (ovrAvatarControllerComponent?)null
                :  (ovrAvatarControllerComponent)Marshal.PtrToStructure(ptr, typeof(ovrAvatarControllerComponent));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarPose_GetLeftControllerComponent")]
        private static extern IntPtr ovrAvatarPose_GetLeftControllerComponent_Native(IntPtr avatar);

        public static ovrAvatarControllerComponent? ovrAvatarPose_GetRightControllerComponent(
            IntPtr avatar)
        {
            IntPtr ptr = ovrAvatarPose_GetRightControllerComponent_Native(avatar);
            return ptr == IntPtr.Zero
                ?  (ovrAvatarControllerComponent?)null
                :  (ovrAvatarControllerComponent)Marshal.PtrToStructure(ptr, typeof(ovrAvatarControllerComponent));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarPose_GetRightControllerComponent")]
        private static extern IntPtr ovrAvatarPose_GetRightControllerComponent_Native(IntPtr avatar);

        public static ovrAvatarHandComponent? ovrAvatarPose_GetLeftHandComponent(
            IntPtr avatar)
        {
            IntPtr ptr = ovrAvatarPose_GetLeftHandComponent_Native(avatar);
            return ptr == IntPtr.Zero
                ?  (ovrAvatarHandComponent?)null
                :  (ovrAvatarHandComponent)Marshal.PtrToStructure(ptr, typeof(ovrAvatarHandComponent));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarPose_GetLeftHandComponent")]
        private static extern IntPtr ovrAvatarPose_GetLeftHandComponent_Native(IntPtr avatar);

        public static ovrAvatarHandComponent? ovrAvatarPose_GetRightHandComponent(
            IntPtr avatar)
        {
            IntPtr ptr = ovrAvatarPose_GetRightHandComponent_Native(avatar);
            return ptr == IntPtr.Zero
                ?  (ovrAvatarHandComponent?)null
                :  (ovrAvatarHandComponent)Marshal.PtrToStructure(ptr, typeof(ovrAvatarHandComponent));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarPose_GetRightHandComponent")]
        private static extern IntPtr ovrAvatarPose_GetRightHandComponent_Native(IntPtr avatar);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatarAsset_BeginLoading(UInt64 assetID);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern ovrAvatarAssetType ovrAvatarAsset_GetType(IntPtr assetHandle);

        public static ovrAvatarMeshAssetData ovrAvatarAsset_GetMeshData(
            IntPtr assetPtr)
        {
            IntPtr ptr = ovrAvatarAsset_GetMeshData_Native(assetPtr);
            return (ovrAvatarMeshAssetData)Marshal.PtrToStructure(
                ptr, typeof(ovrAvatarMeshAssetData));
        }
        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarAsset_GetMeshData")]
        private static extern IntPtr ovrAvatarAsset_GetMeshData_Native(IntPtr assetPtr);

        public static ovrAvatarTextureAssetData ovrAvatarAsset_GetTextureData(
            IntPtr assetPtr)
        {
            IntPtr ptr = ovrAvatarAsset_GetTextureData_Native(assetPtr);
            return (ovrAvatarTextureAssetData)Marshal.PtrToStructure(
                ptr, typeof(ovrAvatarTextureAssetData));
        }
        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarAsset_GetTextureData")]
        private static extern IntPtr ovrAvatarAsset_GetTextureData_Native(IntPtr assetPtr);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint =
            "ovrAvatarAsset_GetMaterialData")]
        private static extern IntPtr ovrAvatarAsset_GetMaterialData_Native(IntPtr assetPtr);
        public static ovrAvatarMaterialState ovrAvatarAsset_GetMaterialState(IntPtr assetPtr)
        {
            IntPtr ptr = ovrAvatarAsset_GetMaterialData_Native(assetPtr);
            return (ovrAvatarMaterialState)Marshal.PtrToStructure(ptr, typeof(ovrAvatarMaterialState));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern ovrAvatarRenderPartType ovrAvatarRenderPart_GetType(IntPtr renderPart);

        public static ovrAvatarRenderPart_SkinnedMeshRender ovrAvatarRenderPart_GetSkinnedMeshRender(IntPtr renderPart)
        {
            IntPtr ptr = ovrAvatarRenderPart_GetSkinnedMeshRender_Native(renderPart);
            return (ovrAvatarRenderPart_SkinnedMeshRender)Marshal.PtrToStructure(
                ptr, typeof(ovrAvatarRenderPart_SkinnedMeshRender));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint = "ovrAvatarRenderPart_GetSkinnedMeshRender")]
        private static extern IntPtr ovrAvatarRenderPart_GetSkinnedMeshRender_Native(IntPtr renderPart);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern ovrAvatarTransform ovrAvatarSkinnedMeshRender_GetTransform(IntPtr renderPart);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern ovrAvatarTransform ovrAvatarSkinnedMeshRenderPBS_GetTransform(IntPtr renderPart);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern ovrAvatarVisibilityFlags ovrAvatarSkinnedMeshRender_GetVisibilityMask(IntPtr renderPart);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern bool ovrAvatarSkinnedMeshRender_MaterialStateChanged(IntPtr renderPart);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern ovrAvatarVisibilityFlags ovrAvatarSkinnedMeshRenderPBS_GetVisibilityMask(IntPtr renderPart);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern ovrAvatarMaterialState ovrAvatarSkinnedMeshRender_GetMaterialState(IntPtr renderPart);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern UInt64 ovrAvatarSkinnedMeshRender_GetDirtyJoints(IntPtr renderPart);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern UInt64 ovrAvatarSkinnedMeshRenderPBS_GetDirtyJoints(IntPtr renderPart);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern ovrAvatarTransform ovrAvatarSkinnedMeshRender_GetJointTransform(IntPtr renderPart, UInt32 jointIndex);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern ovrAvatarTransform ovrAvatarSkinnedMeshRenderPBS_GetJointTransform(IntPtr renderPart, UInt32 jointIndex);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern UInt64 ovrAvatarSkinnedMeshRenderPBS_GetAlbedoTextureAssetID(IntPtr renderPart);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern UInt64 ovrAvatarSkinnedMeshRenderPBS_GetSurfaceTextureAssetID(IntPtr renderPart);

        public static ovrAvatarRenderPart_SkinnedMeshRenderPBS ovrAvatarRenderPart_GetSkinnedMeshRenderPBS(IntPtr renderPart)
        {
            IntPtr ptr = ovrAvatarRenderPart_GetSkinnedMeshRenderPBS_Native(renderPart);
            return (ovrAvatarRenderPart_SkinnedMeshRenderPBS)Marshal.PtrToStructure(
                ptr, typeof(ovrAvatarRenderPart_SkinnedMeshRenderPBS));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint = "ovrAvatarRenderPart_GetSkinnedMeshRenderPBS")]
        private static extern IntPtr ovrAvatarRenderPart_GetSkinnedMeshRenderPBS_Native(IntPtr renderPart);

        public static ovrAvatarRenderPart_ProjectorRender ovrAvatarRenderPart_GetProjectorRender(IntPtr renderPart)
        {
            IntPtr ptr = ovrAvatarRenderPart_GetProjectorRender_Native(renderPart);
            return (ovrAvatarRenderPart_ProjectorRender)Marshal.PtrToStructure(
                ptr, typeof(ovrAvatarRenderPart_ProjectorRender));
        }

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl, EntryPoint = "ovrAvatarRenderPart_GetProjectorRender")]
        private static extern IntPtr ovrAvatarRenderPart_GetProjectorRender_Native(IntPtr renderPart);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern UInt32 ovrAvatar_GetReferencedAssetCount(IntPtr avatar);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern UInt64 ovrAvatar_GetReferencedAsset(IntPtr avatar, UInt32 index);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatar_SetLeftHandGesture(IntPtr avatar, ovrAvatarHandGesture gesture);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatar_SetRightHandGesture(IntPtr avatar, ovrAvatarHandGesture gesture);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatar_SetLeftHandCustomGesture(IntPtr avatar, UInt32 jointCount, [In] ovrAvatarTransform[] customJointTransforms);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatar_SetRightHandCustomGesture(IntPtr avatar, UInt32 jointCount, [In] ovrAvatarTransform[] customJointTransforms);

        //Native calls for efficient packet updates
        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatar_UpdatePoseFromPacket(IntPtr avatar, IntPtr packet, float secondsFromStart);
        
        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatarPacket_BeginRecording(IntPtr avatar);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ovrAvatarPacket_EndRecording(IntPtr avatar);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern UInt32 ovrAvatarPacket_GetSize(IntPtr packet);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern float ovrAvatarPacket_GetDurationSeconds(IntPtr packet);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ovrAvatarPacket_Free(IntPtr packet);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern bool ovrAvatarPacket_Write(IntPtr packet, UInt32 bufferSize, [Out] byte[] buffer);

        [DllImport(LibFile, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ovrAvatarPacket_Read(UInt32 bufferSize, [In] byte[] buffer);
    }
}
