using System;
using Oculus.Avatar;
using UnityEngine;
using System.Runtime.InteropServices;

public class OvrAvatarAssetMesh : OvrAvatarAsset {
    public Mesh mesh;
    private ovrAvatarSkinnedMeshPose skinnedBindPose;
    public string[] jointNames;

    public OvrAvatarAssetMesh(UInt64 _assetId, IntPtr asset) {
        assetID = _assetId;
        ovrAvatarMeshAssetData meshAssetData = CAPI.ovrAvatarAsset_GetMeshData(asset);

        mesh = new Mesh();
        mesh.name = "Procedural Geometry for asset " + _assetId;

        long vertexCount = (long)meshAssetData.vertexCount;
        Vector3[] vertices = new Vector3[vertexCount];
        Vector3[] normals = new Vector3[vertexCount];
        Vector4[] tangents = new Vector4[vertexCount];
        Vector2[] uv = new Vector2[vertexCount];
        Color[] colors = new Color[vertexCount];
        BoneWeight[] boneWeights = new BoneWeight[vertexCount];

        long vertexSize = (long)Marshal.SizeOf(typeof(ovrAvatarMeshVertex));
        long vertexBufferStart = meshAssetData.vertexBuffer.ToInt64();

        for (long i = 0; i < vertexCount; i++)
        {
            long offset = vertexSize * i;
            ovrAvatarMeshVertex vertex = (ovrAvatarMeshVertex)Marshal.PtrToStructure(new IntPtr(vertexBufferStart + offset), typeof(ovrAvatarMeshVertex));
            vertices[i] = new Vector3(vertex.x, vertex.y, -vertex.z);
            normals[i] = new Vector3(vertex.nx, vertex.ny, -vertex.nz);
            tangents[i] = new Vector4(vertex.tx, vertex.ty, -vertex.tz, vertex.tw);
            uv[i] = new Vector2(vertex.u, vertex.v);
            colors[i] = new Color(0, 0, 0, 1);

            boneWeights[i].boneIndex0 = vertex.blendIndices[0];
            boneWeights[i].boneIndex1 = vertex.blendIndices[1];
            boneWeights[i].boneIndex2 = vertex.blendIndices[2];
            boneWeights[i].boneIndex3 = vertex.blendIndices[3];
            boneWeights[i].weight0 = vertex.blendWeights[0];
            boneWeights[i].weight1 = vertex.blendWeights[1];
            boneWeights[i].weight2 = vertex.blendWeights[2];
            boneWeights[i].weight3 = vertex.blendWeights[3];
        }
        mesh.vertices = vertices;
        mesh.normals = normals;
        mesh.uv = uv;
        mesh.tangents = tangents;
        mesh.boneWeights = boneWeights;
        mesh.colors = colors;

        skinnedBindPose = meshAssetData.skinnedBindPose;

        ulong indexCount = meshAssetData.indexCount;
        Int16[] indices = new Int16[indexCount];
        IntPtr indexBufferPtr = meshAssetData.indexBuffer;
        Marshal.Copy(indexBufferPtr, indices, 0, (int)indexCount);
        Int32[] triangles = new Int32[indexCount];
        for (ulong i = 0; i < indexCount; i+=3)
        {
            triangles[i+2] = (Int32)indices[i];
            triangles[i+1] = (Int32)indices[i+1];
            triangles[i] = (Int32)indices[i+2];
        }
        mesh.triangles = triangles;

        UInt32 jointCount = skinnedBindPose.jointCount;
        jointNames = new string[jointCount];
        for (UInt32 i = 0; i < jointCount; i++)
        {
            jointNames[i] = Marshal.PtrToStringAnsi(skinnedBindPose.jointNames[i]);
        }
    }

    public SkinnedMeshRenderer CreateSkinnedMeshRendererOnObject(GameObject target)
    {
        SkinnedMeshRenderer skinnedMeshRenderer = target.AddComponent<SkinnedMeshRenderer>();
        skinnedMeshRenderer.sharedMesh = mesh;
        mesh.name = "AvatarMesh_" + assetID;
        UInt32 jointCount = skinnedBindPose.jointCount;
        GameObject[] bones = new GameObject[jointCount];
        Transform[] boneTransforms = new Transform[jointCount];
        Matrix4x4[] bindPoses = new Matrix4x4[jointCount];
        for (UInt32 i = 0; i < jointCount; i++)
        {
            bones[i] = new GameObject();
            boneTransforms[i] = bones[i].transform;
            bones[i].name = jointNames[i];
            int parentIndex = skinnedBindPose.jointParents[i];
            if (parentIndex == -1)
            {
                bones[i].transform.parent = skinnedMeshRenderer.transform;
                skinnedMeshRenderer.rootBone = bones[i].transform;
            }
            else
            {
                bones[i].transform.parent = bones[parentIndex].transform;
            }

            // Set the position relative to the parent
            Vector3 position = skinnedBindPose.jointTransform[i].position;
            position.z = -position.z;
            bones[i].transform.localPosition = position;

            Quaternion orientation = skinnedBindPose.jointTransform[i].orientation;
            orientation.x = -orientation.x;
            orientation.y = -orientation.y;
            bones[i].transform.localRotation = orientation;

            bones[i].transform.localScale = skinnedBindPose.jointTransform[i].scale;

            bindPoses[i] = bones[i].transform.worldToLocalMatrix * skinnedMeshRenderer.transform.localToWorldMatrix;
        }
        skinnedMeshRenderer.bones = boneTransforms;
        mesh.bindposes = bindPoses;
        return skinnedMeshRenderer;
    }
}
