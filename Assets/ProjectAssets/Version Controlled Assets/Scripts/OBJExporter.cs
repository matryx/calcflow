using UnityEngine;
using System.Collections;
using System.IO;
using System.Text;
using System.Threading;

public class ObjExporter
{
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

   public static void SaveMesh(MeshFilter mf, string filename)
    {
        string name = string.Copy(mf.name);
        Vector3[] vertices = mf.mesh.vertices.Clone() as Vector3[];
        Vector3[] normals = mf.mesh.normals.Clone() as Vector3[];
        Vector2[] uvs = mf.mesh.uv.Clone() as Vector2[];
        int[] faces = mf.mesh.triangles.Clone() as int[];

        Thread t = new Thread(() => ThreadedMeshSave(name, vertices, normals, uvs, faces, filename));
        t.Start();
    }

    public static void ThreadedMeshSave(string name, Vector3[] vertices, Vector3[] normals, Vector2[] uvs, int[] faces, string filename)
    {
        StringBuilder sb = new StringBuilder();

        sb.Append("g ").Append(name).Append("\n");
        foreach (Vector3 v in vertices)
        {
            sb.Append(string.Format("v {0} {1} {2}\n", v.x, v.y, v.z));
        }
        sb.Append("\n");
        foreach (Vector3 v in normals)
        {
            sb.Append(string.Format("vn {0} {1} {2}\n", v.x, v.y, v.z));
        }
        sb.Append("\n");
        foreach (Vector3 v in uvs)
        {
            sb.Append(string.Format("vt {0} {1}\n", v.x, v.y));
        }

        for (int i = 0; i < faces.Length; i += 3)
        {
            sb.Append(string.Format("f {0}/{0}/{0} {1}/{1}/{1} {2}/{2}/{2}\n",
                faces[i] + 1, faces[i + 1] + 1, faces[i + 2] + 1));
        }
        string content = sb.ToString();
        using (StreamWriter sw = new StreamWriter(filename))
        {
            sw.Write(content);
        }
    }
}