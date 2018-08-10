using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using MathFunctions;
using Nanome.Core;
using UnityEngine;

public class CustomDensityPlot : MonoBehaviour {
    struct Geometry {
        public Vector3[] vertices;
        public Vector3[] normals;
        public Color32[] vertexColors;
        public int[] faces;
    }

    public Gradient gradient;

    public Material densityMat;
    public Material oldDensity;
    public Boolean oldFunctionality;

    GameObject go;

    List<Transform> vectors;
    List<Vector3> startPts;
    List<Vector3> offsets;
    float max_magnitude;

    public string expressionX;
    public string expressionY;
    public string expressionZ;

    AK.ExpressionSolver solver;
    AK.Expression expX, expY, expZ;
    AK.Variable varX, varY, varZ;
    [HideInInspector]
    public ExpressionSet es;

    public float minmaxVal;
    public float delta = 1f;
    public enum SampleDensity { HIGH, MEDIUM, LOW };
 [HideInInspector]
 public SampleDensity dens;

 float xmin;
 float xmax;
 float ymin;
 float ymax;
 float zmin;
 float zmax;

 Texture3D textureMap;

 // Use this for initialization
 void Start () {
 xmin = -1f * minmaxVal;
 xmax = minmaxVal;
 ymin = -1f * minmaxVal;
 ymax = minmaxVal;
 zmin = -1f * minmaxVal;
 zmax = minmaxVal;
 vectors = new List<Transform> ();
 startPts = new List<Vector3> ();
 offsets = new List<Vector3> ();
 solver = new AK.ExpressionSolver ();
 expX = new AK.Expression ();
 expY = new AK.Expression ();
 expZ = new AK.Expression ();
 solver.SetGlobalVariable ("x", 0);
 solver.SetGlobalVariable ("y", 0);
 solver.SetGlobalVariable ("z", 0);
 varX = solver.GetGlobalVariable ("x");
 varY = solver.GetGlobalVariable ("y");
 varZ = solver.GetGlobalVariable ("z");
 max_magnitude = 0f;
 textureMap = new Texture3D (128, 128, 128, TextureFormat.RGBA32, true);
 textureMap.wrapMode = TextureWrapMode.Clamp;
 //CalculateVectors();
 //DrawDensityPlot();
    }

    // Update is called once per frame
    void Update () {

    }

    // Set point spread and calculate vector values at all points
    void CalculateVectors () {
        max_magnitude = 0f;

        switch (dens) {
            case SampleDensity.LOW:
                delta = (xmax - xmin) / 4.0f;
                break;
            case SampleDensity.MEDIUM:
                delta = (xmax - xmin) / 6.0f;
                break;
            case SampleDensity.HIGH:
                delta = (xmax - xmin) / 127.0f;
                break;
        }

        for (float x_temp = xmin; x_temp < xmax; x_temp += delta) {
            for (float y_temp = ymin; y_temp < ymax; y_temp += delta) {
                for (float z_temp = zmin; z_temp < zmax; z_temp += delta) {
                    varX.value = x_temp;
                    varY.value = z_temp;
                    varZ.value = y_temp;

                    float x = (float) expX.Evaluate ();
                    //Mathf.Clamp(x, -Mathf.Exp(20), Mathf.Exp(20));

                    //float y = (float)expY.Evaluate();
                    //Mathf.Clamp(y, -Mathf.Exp(20), Mathf.Exp(20));

                    //float z = (float)expZ.Evaluate();
                    //Mathf.Clamp(z, -Mathf.Exp(20), Mathf.Exp(20));

                    Vector3 target = new Vector3 (x_temp, y_temp, z_temp);

                    Vector3 result = new Vector3 (x, 0, 0);
                    if (float.IsNaN (x)
                        //   || float.IsNaN(y)
                        //   || float.IsNaN(z)
                        ||
                        result.magnitude == 0) {
                        result = new Vector3 (0, 0, 0);
                    }

                    //Vector3 direction = result.normalized;
                    float length = result.magnitude;
                    if (length > max_magnitude) {
                        max_magnitude = length;
                    }
                    startPts.Add (target);
                    offsets.Add (result);
                }
            }
        }
    }

    // Draw shapes at all points
    void DrawDensityPlot () {
        List<Geometry> rawGeom = new List<Geometry> ();
        Color32[] colors = new Color32[startPts.Count];

        for (int i = 0; i < startPts.Count; i++) {
            Vector3 target = startPts[i];
            Vector3 offset = offsets[i] / max_magnitude;
            //Vector3 tip = offset * 0.4f;

            //Transform l = Instantiate(vPrefab);
            //l.SetParent(transform, false);
            //LineRenderer top = l.Find("Top").GetComponent<LineRenderer>();
            //top.SetPosition(0, target + offset - tip);
            //top.SetPosition(1, target + offset);
            //LineRenderer body = l.Find("Body").GetComponent<LineRenderer>();
            //body.SetPosition(0, target + offset - tip);
            //body.SetPosition(1, target);
            //vectors.Add(l);

            Color32 c = gradient.Evaluate (offset.magnitude);
            colors[i] = c;
            //top.startColor = c;
            //top.endColor = c;
            //body.startColor = c;
            //body.endColor = c;
            //rawGeom.Add(CreateSphere(target, offset, offset.magnitude, offset.magnitude, c));
            
            if(c.a> 0 && oldFunctionality){
                // rawGeom.Add(CreateCone(target, target, 0.3f, 0.15f, c));
                // rawGeom.Add(CreateCone(target, -1 * target, 0.3f, 0.15f, c));
                rawGeom.Add(CreateCube(target, delta, c));
            }
            
            //rawGeom.Add(CreateCone(target, offset, offset.magnitude, offset.magnitude, c));
            //rawGeom.Add(CreateCone(target, -1*offset, offset.magnitude, offset.magnitude, c));
        }

        textureMap.SetPixels32 (colors);
        textureMap.Apply ();
        densityMat.SetTexture ("_VolumeTex", textureMap);

        if (oldFunctionality){
            List<Geometry> combined = CombineGeometry(rawGeom);
            List<Mesh> meshes = new List<Mesh>();
            foreach (var geom in combined)
            {
                Mesh mesh = new Mesh()
                {
                    vertices = geom.vertices,
                    normals = geom.normals,
                    colors32 = geom.vertexColors,
                    triangles = geom.faces
                };
                meshes.Add(mesh);
            }
            go = Instantiate(new GameObject(), transform);
            go.name = "vector field mesh";
            go.transform.localPosition = new Vector3(0, 0, 0);
            go.transform.localRotation = Quaternion.identity;
            for (int i = 0; i < meshes.Count; i++)
            {
                Mesh mesh = meshes[i];
                GameObject g = Instantiate(new GameObject(), go.transform);
                g.name = "submesh" + i;
                g.transform.localPosition = new Vector3(0, 0, 0);
                g.transform.localRotation = Quaternion.identity;
                MeshFilter mf = g.AddComponent<MeshFilter>();
                mf.mesh = mesh;
                MeshRenderer mr = g.AddComponent<MeshRenderer>();
                mr.material = oldDensity;
            }
        }
        
    }

    void Clear () {
        Destroy (go);
        vectors.Clear ();
        startPts.Clear ();
        offsets.Clear ();
    }

    public void UpdateFunctions () {
        Clear ();
        //if (es.CompileAll())
        //{
        es.CompileAll ();
        expX = solver.SymbolicateExpression (es.expressions[ExpressionSet.ExpOptions.X].expression);
        expY = solver.SymbolicateExpression (es.expressions[ExpressionSet.ExpOptions.Y].expression);
        expZ = solver.SymbolicateExpression (es.expressions[ExpressionSet.ExpOptions.Z].expression);
        //}
        //else
        //{
        //    return;
        //}
        //try
        //{
        //    expX = solver.SymbolicateExpression(expressionX);
        //    expY = solver.SymbolicateExpression(expressionY);
        //    expZ = solver.SymbolicateExpression(expressionZ);
        //}
        //catch
        //{
        //    return;
        //}
        CalculateVectors ();
        DrawDensityPlot ();
    }

    private Geometry CreateCube (Vector3 position, float length, Color32 color) {
        int vertNum = 8; // top & bottom center
        float x = position.x;
        float y = position.y;
        float z = position.z;

        Vector3[] vertices = {
            new Vector3 (x, y, z),
            new Vector3 (length + x, y, z),
            new Vector3 (length + x, length + y, z),
            new Vector3 (x, length + y, z),
            new Vector3 (x, length + y, length + z),
            new Vector3 (length + x, length + y, length + z),
            new Vector3 (length + x, y, length + z),
            new Vector3 (x, y, length + z),
        };

        int[] faces = {
            0,
            2,
            1, //face front
            0,
            3,
            2,
            2,
            3,
            4, //face top
            2,
            4,
            5,
            1,
            2,
            5, //face right
            1,
            5,
            6,
            0,
            7,
            4, //face left
            0,
            4,
            3,
            5,
            4,
            7, //face back
            5,
            7,
            6,
            0,
            6,
            7, //face bottom
            0,
            1,
            6
        };

        var normals = new Vector3[vertNum];
        var colors = new Color32[vertNum];

        normals[0] = Vector3.Cross ((vertices[1] - vertices[0]), (vertices[2] - vertices[0])) / Vector3.Cross ((vertices[1] - vertices[0]), (vertices[2] - vertices[0])).magnitude;
        normals[1] = Vector3.Cross ((vertices[0] - vertices[1]), (vertices[2] - vertices[1])) / Vector3.Cross ((vertices[0] - vertices[1]), (vertices[2] - vertices[1])).magnitude;
        normals[2] = Vector3.Cross ((vertices[0] - vertices[2]), (vertices[1] - vertices[2])) / Vector3.Cross ((vertices[0] - vertices[2]), (vertices[1] - vertices[2])).magnitude;
        normals[3] = Vector3.Cross ((vertices[4] - vertices[3]), (vertices[2] - vertices[3])) / Vector3.Cross ((vertices[4] - vertices[3]), (vertices[2] - vertices[3])).magnitude;
        normals[4] = Vector3.Cross ((vertices[3] - vertices[4]), (vertices[2] - vertices[4])) / Vector3.Cross ((vertices[3] - vertices[4]), (vertices[2] - vertices[4])).magnitude;
        normals[5] = Vector3.Cross ((vertices[6] - vertices[5]), (vertices[7] - vertices[5])) / Vector3.Cross ((vertices[6] - vertices[5]), (vertices[7] - vertices[5])).magnitude;
        normals[6] = Vector3.Cross ((vertices[5] - vertices[6]), (vertices[7] - vertices[6])) / Vector3.Cross ((vertices[5] - vertices[6]), (vertices[7] - vertices[6])).magnitude;
        normals[7] = Vector3.Cross ((vertices[5] - vertices[7]), (vertices[6] - vertices[7])) / Vector3.Cross ((vertices[5] - vertices[7]), (vertices[6] - vertices[7])).magnitude;

        for (int i = 0; i < vertNum; i++) {
            colors[i] = color;
        }

        Geometry geom = new Geometry () {
            vertices = vertices,
            normals = normals,
            vertexColors = colors,
            faces = faces
        };
        return geom;
    }

    private Geometry CreateCylinder (Vector3 position, Vector3 dir, float length, float radius, Color32 color, int tessel = 25) {
        int vertNum = tessel * 2 + 2; // top & bottom center
        int triNum = tessel * 4; // top circle + bottom circle + triangle strip in the middle

        var vertices = new Vector3[vertNum];
        var normals = new Vector3[vertNum];
        var colors = new Color32[vertNum];
        var faces = new int[triNum * 3];

        Matrix4x4 prefix = Matrix4x4.TRS (Vector3.zero, Quaternion.LookRotation (Vector3.up, Vector3.forward), Vector3.one);
        Matrix4x4 trs = Matrix4x4.TRS (position, Quaternion.LookRotation (dir, Vector3.up), Vector3.one) * prefix;

        Vector3 bottom = new Vector3 (0, 0, 0);
        Vector3 top = new Vector3 (0, length, 0);
        vertices[0] = trs.MultiplyPoint (bottom);
        vertices[1] = trs.MultiplyPoint (top);
        normals[0] = trs.MultiplyVector (Vector3.down);
        normals[1] = trs.MultiplyVector (Vector3.up);
        colors[0] = color;
        colors[1] = color;

        int index = 2;
        int tri = 0;
        int i0 = 0, i1 = 0, i2 = 0, i3 = 0;
        for (int i = 0; i < tessel; i++) {
            //index += 2;
            float cos = Mathf.Cos ((float) i / (tessel - 1) * 2.0f * Mathf.PI);
            float sin = Mathf.Sin ((float) i / (tessel - 1) * 2.0f * Mathf.PI);
            vertices[index] = trs.MultiplyPoint (new Vector3 (cos * radius, 0, sin * radius));
            normals[index] = trs.MultiplyVector (new Vector3 (cos, 0, sin));
            colors[index] = color;
            vertices[index + 1] = trs.MultiplyPoint (new Vector3 (cos * radius, length, sin * radius));
            normals[index + 1] = trs.MultiplyVector (new Vector3 (cos, 0, sin));
            colors[index + 1] = color;
            if (i == 0) {
                index += 2;
                continue;
            }
            i0 = index - 2;
            i1 = index - 1;
            i2 = index + 1;
            i3 = index;
            // triangle strip
            faces[tri++] = i0;
            faces[tri++] = i1;
            faces[tri++] = i2;
            faces[tri++] = i0;
            faces[tri++] = i2;
            faces[tri++] = i3;
            // bottom triangle
            faces[tri++] = 0;
            faces[tri++] = i0;
            faces[tri++] = i3;
            // top triangle
            faces[tri++] = 1;
            faces[tri++] = i2;
            faces[tri++] = i1;
            index += 2;
            //index += 2;
        }
        i0 = index;
        i1 = index + 1;
        i2 = 3;
        i3 = 2;
        // triangle strip
        faces[tri++] = i0;
        faces[tri++] = i1;
        faces[tri++] = i2;
        faces[tri++] = i0;
        faces[tri++] = i2;
        faces[tri++] = i3;
        // bottom triangle
        faces[tri++] = 0;
        faces[tri++] = i0;
        faces[tri++] = i3;
        // top triangle
        faces[tri++] = 1;
        faces[tri++] = i2;
        faces[tri++] = i1;

        Geometry geom = new Geometry () {
            vertices = vertices,
            normals = normals,
            vertexColors = colors,
            faces = faces
        };
        return geom;
    }

    private Geometry CreateCone (Vector3 position, Vector3 dir, float length, float radius, Color32 color, int tessel = 25) {
        int vertNum = tessel + 2;
        int triNum = tessel * 2;

        var vertices = new Vector3[vertNum];
        var normals = new Vector3[vertNum];
        var colors = new Color32[vertNum];
        var faces = new int[triNum * 3];

        Matrix4x4 prefix = Matrix4x4.TRS (Vector3.zero, Quaternion.LookRotation (Vector3.up, Vector3.forward), Vector3.one);
        Matrix4x4 trs = Matrix4x4.TRS (position, Quaternion.LookRotation (dir, Vector3.up), Vector3.one) * prefix;

        Vector3 bottom = new Vector3 (0, 0, 0);
        Vector3 top = new Vector3 (0, length, 0);
        vertices[0] = trs.MultiplyPoint (bottom);
        vertices[1] = trs.MultiplyPoint (top);
        normals[0] = trs.MultiplyVector (Vector3.down);
        normals[1] = trs.MultiplyVector (Vector3.up);
        colors[0] = color;
        colors[1] = color;

        int index = 1;
        int tri = 0;
        for (int i = 0; i < tessel; i++) {
            index++;
            float cos = Mathf.Cos ((float) i / (tessel - 1) * 2.0f * Mathf.PI);
            float sin = Mathf.Sin ((float) i / (tessel - 1) * 2.0f * Mathf.PI);
            vertices[index] = trs.MultiplyPoint (new Vector3 (cos * radius, 0, sin * radius));
            normals[index] = trs.MultiplyVector (new Vector3 (cos, 0, sin));
            colors[index] = color;
            if (i == 0) continue;
            int i0 = index - 1;
            int i1 = index;
            faces[tri++] = i0;
            faces[tri++] = 1;
            faces[tri++] = i1;
            faces[tri++] = i0;
            faces[tri++] = i1;
            faces[tri++] = 0;
        }
        Geometry geom = new Geometry () {
            vertices = vertices,
            normals = normals,
            vertexColors = colors,
            faces = faces
        };
        return geom;
    }

    private List<Geometry> CombineGeometry (List<Geometry> geoms, int maxVertices = 64000) {
        int combinedIndex = 0;
        List<Geometry> combined = new List<Geometry> ();
        if (geoms.Count == 1) {
            return geoms;
        }
        while (combinedIndex < geoms.Count) {
            int vertices_to_combine = 0;
            int faces_to_combine = 0;
            int mesh_to_combine = 0;
            for (mesh_to_combine = combinedIndex; mesh_to_combine < geoms.Count; mesh_to_combine++) {
                Geometry geom = geoms[mesh_to_combine];
                if (vertices_to_combine + geom.vertices.Length > maxVertices) {
                    break;
                }
                vertices_to_combine += geom.vertices.Length;
                faces_to_combine += geom.faces.Length;
            }
            // if (mesh_to_combine == 1)
            // {
            //     combined.Add(geoms[combinedIndex++]);
            //     continue;
            // }
            Geometry newGeom = new Geometry () {
                vertices = new Vector3[vertices_to_combine],
                normals = new Vector3[vertices_to_combine],
                vertexColors = new Color32[vertices_to_combine],
                faces = new int[faces_to_combine]
            };
            int tri = 0;
            int vert = 0;
            for (int i = combinedIndex; i < mesh_to_combine; i++) {
                Geometry geom = geoms[i];
                geom.vertices.CopyTo (newGeom.vertices, vert);
                geom.normals.CopyTo (newGeom.normals, vert);
                geom.vertexColors.CopyTo (newGeom.vertexColors, vert);
                geom.faces.CopyTo (newGeom.faces, tri);
                if (vert != 0) {
                    for (int j = 0; j < geom.faces.Length; j++) {
                        newGeom.faces[tri + j] += vert;
                    }
                }
                vert += geom.vertices.Length;
                tri += geom.faces.Length;

            }
            combinedIndex = mesh_to_combine;
            combined.Add (newGeom);
        }
        return combined;
    }

}