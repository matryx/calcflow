using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using Nanome.Core;
using UnityEngine;

public class ObjExporter {
    //public static string MeshToString(MeshFilter mf)
    //{
    //    Mesh m = mf.mesh;
    //    //Material[] mats = mf.renderer.sharedMaterials;
    //    Material[] mats = mf.GetComponent<MeshRenderer>().sharedMaterials;

    //    StringBuilder sb = new StringBuilder();

    //    sb.Append("g ").Append(mf.name).Append("\n");
    //    foreach (Vector3 v in m.vertices)
    //    {
    //        sb.Append(string.Format("v {0} {1} {2}\n", v.x, v.y, v.z));
    //    }
    //    sb.Append("\n");
    //    foreach (Vector3 v in m.normals)
    //    {
    //        sb.Append(string.Format("vn {0} {1} {2}\n", v.x, v.y, v.z));
    //    }
    //    sb.Append("\n");
    //    foreach (Vector3 v in m.uv)
    //    {
    //        sb.Append(string.Format("vt {0} {1}\n", v.x, v.y));
    //    }
    //    for (int material = 0; material < m.subMeshCount; material++)
    //    {
    //        sb.Append("\n");
    //        sb.Append("usemtl ").Append(mats[material].name).Append("\n");
    //        sb.Append("usemap ").Append(mats[material].name).Append("\n");

    //        int[] triangles = m.GetTriangles(material);
    //        for (int i = 0; i < triangles.Length; i += 3)
    //        {
    //            sb.Append(string.Format("f {0}/{0}/{0} {1}/{1}/{1} {2}/{2}/{2}\n",
    //                triangles[i] + 1, triangles[i + 1] + 1, triangles[i + 2] + 1));
    //        }
    //    }
    //    return sb.ToString();
    //}

    //public static void MeshToFile(MeshFilter mf, string filename)
    //{
    //    using (StreamWriter sw = new StreamWriter(filename))
    //    {
    //        sw.Write(MeshToString(mf));
    //    }
    //}

    // Sets up the name, vertices, normals, uvs, and triangles in one
    // unified list to send to the saving function
    public static void SaveMesh (List<MeshFilter> mfs, string filename) {

        string name = string.Format ("CreatedMesh");

        int verticesSize = 0, normalsSize = 0, uvsSize = 0, facesSize = 0;

        for (int i = 0; i < mfs.Count; i++) {
            verticesSize += mfs[i].mesh.vertices.Length;
            normalsSize += mfs[i].mesh.normals.Length;
            uvsSize += mfs[i].mesh.uv.Length;
            facesSize += mfs[i].mesh.triangles.Length;
        }

        Vector3[] vertices = new Vector3[verticesSize];
        Vector3[] normals = new Vector3[normalsSize];
        Vector2[] uvs = new Vector2[uvsSize];
        int[] faces = new int[facesSize];
        mfs[0].mesh.vertices.CopyTo (vertices, 0);
        mfs[0].mesh.normals.CopyTo (normals, 0);
        mfs[0].mesh.uv.CopyTo (uvs, 0);
        mfs[0].mesh.triangles.CopyTo (faces, 0);
        int numFinished = 1;
        if (mfs.Count == 1){
            Thread t = new Thread (() => ThreadedMeshSave (name, vertices, normals, uvs, faces, filename));
            t.Start ();
        }
        for (int i = 1; i < mfs.Count; i++) {
            int m = i;
            mfs[i].mesh.vertices.CopyTo (vertices, mfs[i - 1].mesh.vertices.Length);
            mfs[i].mesh.normals.CopyTo (normals, mfs[i - 1].mesh.normals.Length);
            mfs[i].mesh.uv.CopyTo (uvs, mfs[i - 1].mesh.uv.Length);
            int[] shiftFaces = mfs[i].mesh.triangles.Clone () as int[];
            int offsetVal = mfs[i-1].mesh.vertices.Length;
            Async.runInThread ((Async thread) => {
                for (int sf = 0; sf < shiftFaces.Length; sf++) {
                    shiftFaces[sf] += offsetVal;
                }
                thread.pushEvent ("Finished", null);
            }).onEvent ("Finished", (object data) => {
                shiftFaces.CopyTo (faces, mfs[m - 1].mesh.triangles.Length);
                numFinished++;
                if (numFinished == mfs.Count){
                    Thread t = new Thread (() => ThreadedMeshSave (name, vertices, normals, uvs, faces, filename));
                    t.Start ();
                }
            });
        }
    }

    // Formats and then writes the data to the designated file
    public static void ThreadedMeshSave (string name, Vector3[] vertices, Vector3[] normals, Vector2[] uvs, int[] faces, string filename) {
        StringBuilder sb = new StringBuilder ();

        sb.Append ("g ").Append (name).Append ("\n");
        foreach (Vector3 v in vertices) {
            sb.Append (string.Format ("v {0} {1} {2}\n", v.x, v.y, v.z));
        }
        sb.Append ("\n");
        foreach (Vector3 v in normals) {
            sb.Append (string.Format ("vn {0} {1} {2}\n", v.x, v.y, v.z));
        }
        sb.Append ("\n");
        foreach (Vector3 v in uvs) {
            sb.Append (string.Format ("vt {0} {1}\n", v.x, v.y));
        }

        for (int i = 0; i < faces.Length; i += 3) {
            sb.Append (string.Format ("f {0}/{0}/{0} {1}/{1}/{1} {2}/{2}/{2}\n",
                faces[i] + 1, faces[i + 1] + 1, faces[i + 2] + 1));
        }
        string content = sb.ToString ();
        using (StreamWriter sw = new StreamWriter (filename)) {
            sw.Write (content);
        }
    }
}