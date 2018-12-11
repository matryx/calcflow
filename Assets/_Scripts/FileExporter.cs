using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using Nanome.Core;
using UnityEngine;

public class FileExporter {
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

    /* Sets up the name, vertices, normals, uvs, and triangles in unified lists to send 
     * to the saving function as a file of the given format */
    public static void SaveMesh (List<MeshFilter> mfs, string filename, string filetype) {

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

        int offsetVertices = 0;
        int offsetNormals = 0;
        int offsetUvs = 0;
        int offsetFaces = 0;

        if (mfs.Count == 1){
            switch (filetype.ToLower()) {
                default:
                    Debug.LogError("Unsupported filetype: " + filetype);
                    break;
                case "stl":
                    Thread tstl = new Thread (() => ThreadedMeshSaveStl (vertices, normals, faces, filename));
                    tstl.Start ();
                    break;
                case "obj":
                    Thread tobj = new Thread (() => ThreadedMeshSaveObj (name, vertices, normals, uvs, faces, filename));
                    tobj.Start ();
                    break;
            }
        }
        for (int i = 1; i < mfs.Count; i++) {
            offsetVertices += mfs[i-1].mesh.vertices.Length;
            offsetNormals += mfs[i-1].mesh.normals.Length;
            offsetUvs += mfs[i-1].mesh.uv.Length;
            offsetFaces += mfs[i-1].mesh.triangles.Length;
            int saveOffsetFaces = offsetFaces;
            int saveOffsetVert = offsetVertices;
            mfs[i].mesh.vertices.CopyTo (vertices, offsetVertices);
            mfs[i].mesh.normals.CopyTo (normals, offsetNormals);
            mfs[i].mesh.uv.CopyTo (uvs, offsetUvs);
            int[] shiftFaces = mfs[i].mesh.triangles.Clone () as int[];

            Async.runInThread ((Async thread) => {
                for (int sf = 0; sf < shiftFaces.Length; sf++) {
                    shiftFaces[sf] += saveOffsetVert;
                }
                thread.pushEvent ("Finished", null);
            }).onEvent ("Finished", (object data) => {
                shiftFaces.CopyTo (faces, saveOffsetFaces);
                numFinished++;
                if (numFinished == mfs.Count){
                    switch (filetype.ToLower()) {
                        default:
                            Debug.LogError("Unsupported filetype: " + filetype);
                            break;
                        case "stl":
                            Thread tstl = new Thread (() => ThreadedMeshSaveStl (vertices, normals, faces, filename));
                            tstl.Start ();
                            break;
                        case "obj":
                            Thread tobj = new Thread (() => ThreadedMeshSaveObj (name, vertices, normals, uvs, faces, filename));
                            tobj.Start ();
                            break;
                    }
                }
            });
        }
    }

    // Formats and then writes the data to the designated filepath as an obj file
    public static void ThreadedMeshSaveObj (string name, Vector3[] vertices, Vector3[] normals, Vector2[] uvs, int[] faces, string filename) {
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
        using (StreamWriter sw = new StreamWriter (filename + ".obj")) {
            sw.Write (content);
        }
    }

    // Formats and then writes the given data to the designated filepath as a Binary StL file
    public static void ThreadedMeshSaveStl (Vector3[] vertices, Vector3[] normals, int[] faces, string filename) {
        uint numFacets = (uint) (faces.Length/3);
        Vector3 triangleNormal;
        using (BinaryWriter bw = new BinaryWriter (System.IO.File.Open(filename + ".stl", FileMode.Create), new ASCIIEncoding())) {           
            bw.Write(new byte[80]);
            bw.Write(numFacets);
            for (int i = 0; i < faces.Length; i += 3) {
                triangleNormal = CalculateTriangleNormal(normals[faces[i]], normals[faces[i+1]], normals[faces[i+2]]);
                bw.Write(triangleNormal.x);
                bw.Write(triangleNormal.y);
                bw.Write(triangleNormal.z);

                bw.Write(vertices[faces[i]].x);
                bw.Write(vertices[faces[i]].y);
                bw.Write(vertices[faces[i]].z);

                bw.Write(vertices[faces[i+1]].x);
                bw.Write(vertices[faces[i+1]].y);
                bw.Write(vertices[faces[i+1]].z);

                bw.Write(vertices[faces[i+2]].x);
                bw.Write(vertices[faces[i+2]].y);
                bw.Write(vertices[faces[i+2]].z);

                bw.Write((ushort)0);
            }
        }
    }

    /* Calculates the value of the normal of a triangle face by averaging the value
     *  of its vertex normals */
    private static Vector3 CalculateTriangleNormal(Vector3 a, Vector3 b, Vector3 c) {
        return new Vector3((a.x + b.x + c.x)/3f, (a.y + b.y + c.y)/3f, (a.z + b.z + c.z)/3f);
    }

}