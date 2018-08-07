using UnityEngine;
using System;
using System.Collections;
using System.Collections.Generic;
using MathFunctions;

public class CustomVectorField : MonoBehaviour
{
    struct Geometry
    {
        public Vector3[] vertices;
        public Vector3[] normals;
        public Color32[] vertexColors;
        public int[] faces;
    }

    public static CustomVectorField _instance;
    public Gradient gradient;

    public Material arrowMat;
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
    public ExpressionSet es;

    public List<ExpressionSet> expressionSets = new List<ExpressionSet>();

    public float xmin = -4.5f;
    public float xmax = 4.5f;
    public float ymin = -4.5f;
    public float ymax = 4.5f;
    public float zmin = -4.5f;
    public float zmax = 4.5f;
    public float delta = 1f;
    public enum SampleDensity { HIGH, MEDIUM, LOW };
    public SampleDensity dens = SampleDensity.LOW;

    private void Awake()
    {
        _instance = this;
    }

    // Use this for initialization
    void Start()
    {
        vectors = new List<Transform>();
        startPts = new List<Vector3>();
        offsets = new List<Vector3>();
        solver = new AK.ExpressionSolver();
        expX = new AK.Expression();
        expY = new AK.Expression();
        expZ = new AK.Expression();
        solver.SetGlobalVariable("x", 0); solver.SetGlobalVariable("y", 0); solver.SetGlobalVariable("z", 0);
        varX = solver.GetGlobalVariable("x"); varY = solver.GetGlobalVariable("y"); varZ = solver.GetGlobalVariable("z");
        max_magnitude = 0f;
        //CalculateVectors();
        //DrawVectorField();
    }

    // Update is called once per frame
    void Update()
    {

    }

    void CreateExpressionSet()
    {
        expressionSets.Add(new ExpressionSet());
    }

    void UpdateExpressionSet(List<ExpressionSet> expSet)
    {
        expressionSets = expSet;
    }

    void RemoveExpressionSet(int index)
    {
        if (index < expressionSets.Count)
        {
            expressionSets.RemoveAt(index);
        }
    }

    //public void SetES(ExpressionSet ES)
    //{
    //    es = ES;
    //}

    void CalculateVectors()
    {
        max_magnitude = 0f;

        switch (dens)
        {
            case SampleDensity.LOW:
                delta = (xmax - xmin) / 4.0f;
                break;
            case SampleDensity.MEDIUM:
                delta = (xmax - xmin) / 6.0f;
                break;
            case SampleDensity.HIGH:
                delta = (xmax - xmin) / 9.0f;
                break;
        }

        for (float x_temp = xmin; x_temp <= xmax; x_temp += delta)
        {
            for (float y_temp = ymin; y_temp <= ymax; y_temp += delta)
            {
                for (float z_temp = zmin; z_temp <= zmax; z_temp += delta)
                {
                    varX.value = (x_temp);
                    varY.value = (z_temp);
                    varZ.value = (y_temp);

                    float x = (float)expX.Evaluate();
                    //Mathf.Clamp(x, -Mathf.Exp(20), Mathf.Exp(20));

                    float y = (float)expY.Evaluate();
                    //Mathf.Clamp(y, -Mathf.Exp(20), Mathf.Exp(20));

                    float z = (float)expZ.Evaluate();
                    //Mathf.Clamp(z, -Mathf.Exp(20), Mathf.Exp(20));


                    Vector3 target = new Vector3(x_temp, y_temp, z_temp);

                    Vector3 result = new Vector3(x, z, y);
                    if (float.IsNaN(x)
                        || float.IsNaN(y)
                        || float.IsNaN(z)
                        || result.magnitude == 0)
                    {
                        continue;
                    }

                    //Vector3 direction = result.normalized;
                    float length = result.magnitude;
                    if (length > max_magnitude)
                    {
                        max_magnitude = length;
                    }
                    startPts.Add(target);
                    offsets.Add(result);
                }
            }
        }
    }

    void DrawVectorField()
    {
        List<Geometry> rawGeom = new List<Geometry>();

        for (int i = 0; i < startPts.Count; i++)
        {
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

            Color32 c = gradient.Evaluate(offset.magnitude);
            //top.startColor = c;
            //top.endColor = c;
            //body.startColor = c;
            //body.endColor = c;
            rawGeom.Add(CreateCylinder(target, offset, offset.magnitude, 0.05f, c));
            rawGeom.Add(CreateCone(target + offset, offset, 0.3f, 0.15f, c));
        }

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
            mr.material = arrowMat;
        }
    }

    void Clear()
    {
        Destroy(go);
        vectors.Clear();
        startPts.Clear();
        offsets.Clear();
    }

    public void UpdateFunctions()
    {
        Clear();
        //if (es.CompileAll())
        //{
        es.CompileAll();
        expX = solver.SymbolicateExpression(es.GetExpression("X").expression);
        expY = solver.SymbolicateExpression(es.GetExpression("Y").expression);
        expZ = solver.SymbolicateExpression(es.GetExpression("Z").expression);
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
        CalculateVectors();
        DrawVectorField();
    }

    Geometry CreateCylinder(Vector3 position, Vector3 dir, float length, float radius, Color32 color, int tessel = 25)
    {
        int vertNum = tessel * 2 + 2; // top & bottom center
        int triNum = tessel * 4; // top circle + bottom circle + triangle strip in the middle

        var vertices = new Vector3[vertNum];
        var normals = new Vector3[vertNum];
        var colors = new Color32[vertNum];
        var faces = new int[triNum * 3];

        Matrix4x4 prefix = Matrix4x4.TRS(Vector3.zero, Quaternion.LookRotation(Vector3.up, Vector3.forward), Vector3.one);
        Matrix4x4 trs = Matrix4x4.TRS(position, Quaternion.LookRotation(dir, Vector3.up), Vector3.one) * prefix;

        Vector3 bottom = new Vector3(0, 0, 0);
        Vector3 top = new Vector3(0, length, 0);
        vertices[0] = trs.MultiplyPoint(bottom); vertices[1] = trs.MultiplyPoint(top);
        normals[0] = trs.MultiplyVector(Vector3.down); normals[1] = trs.MultiplyVector(Vector3.up);
        colors[0] = color; colors[1] = color;

        int index = 2;
        int tri = 0;
        int i0 = 0, i1 = 0, i2 = 0, i3 = 0;
        for (int i = 0; i < tessel; i++)
        {
            //index += 2;
            float cos = Mathf.Cos((float)i / (tessel - 1) * 2.0f * Mathf.PI);
            float sin = Mathf.Sin((float)i / (tessel - 1) * 2.0f * Mathf.PI);
            vertices[index] = trs.MultiplyPoint(new Vector3(cos * radius, 0, sin * radius));
            normals[index] = trs.MultiplyVector(new Vector3(cos, 0, sin));
            colors[index] = color;
            vertices[index + 1] = trs.MultiplyPoint(new Vector3(cos * radius, length, sin * radius));
            normals[index + 1] = trs.MultiplyVector(new Vector3(cos, 0, sin));
            colors[index + 1] = color;
            if (i == 0)
            {
                index += 2;
                continue;
            }
            i0 = index - 2;
            i1 = index - 1;
            i2 = index + 1;
            i3 = index;
            // triangle strip
            faces[tri++] = i0; faces[tri++] = i1; faces[tri++] = i2;
            faces[tri++] = i0; faces[tri++] = i2; faces[tri++] = i3;
            // bottom triangle
            faces[tri++] = 0; faces[tri++] = i0; faces[tri++] = i3;
            // top triangle
            faces[tri++] = 1; faces[tri++] = i2; faces[tri++] = i1;
            index += 2;
            //index += 2;
        }
        i0 = index; i1 = index + 1; i2 = 3; i3 = 2;
        // triangle strip
        faces[tri++] = i0; faces[tri++] = i1; faces[tri++] = i2;
        faces[tri++] = i0; faces[tri++] = i2; faces[tri++] = i3;
        // bottom triangle
        faces[tri++] = 0; faces[tri++] = i0; faces[tri++] = i3;
        // top triangle
        faces[tri++] = 1; faces[tri++] = i2; faces[tri++] = i1;

        Geometry geom = new Geometry()
        {
            vertices = vertices,
            normals = normals,
            vertexColors = colors,
            faces = faces
        };
        return geom;
    }

    Geometry CreateCone(Vector3 position, Vector3 dir, float length, float radius, Color32 color, int tessel = 25)
    {
        int vertNum = tessel + 2;
        int triNum = tessel * 2;

        var vertices = new Vector3[vertNum];
        var normals = new Vector3[vertNum];
        var colors = new Color32[vertNum];
        var faces = new int[triNum * 3];

        Matrix4x4 prefix = Matrix4x4.TRS(Vector3.zero, Quaternion.LookRotation(Vector3.up, Vector3.forward), Vector3.one);
        Matrix4x4 trs = Matrix4x4.TRS(position, Quaternion.LookRotation(dir, Vector3.up), Vector3.one) * prefix;

        Vector3 bottom = new Vector3(0, 0, 0);
        Vector3 top = new Vector3(0, length, 0);
        vertices[0] = trs.MultiplyPoint(bottom); vertices[1] = trs.MultiplyPoint(top);
        normals[0] = trs.MultiplyVector(Vector3.down); normals[1] = trs.MultiplyVector(Vector3.up);
        colors[0] = color; colors[1] = color;

        int index = 1;
        int tri = 0;
        for (int i = 0; i < tessel; i++)
        {
            index++;
            float cos = Mathf.Cos((float)i / (tessel - 1) * 2.0f * Mathf.PI);
            float sin = Mathf.Sin((float)i / (tessel - 1) * 2.0f * Mathf.PI);
            vertices[index] = trs.MultiplyPoint(new Vector3(cos * radius, 0, sin * radius));
            normals[index] = trs.MultiplyVector(new Vector3(cos, 0, sin));
            colors[index] = color;
            if (i == 0) continue;
            int i0 = index - 1;
            int i1 = index;
            faces[tri++] = i0; faces[tri++] = 1; faces[tri++] = i1;
            faces[tri++] = i0; faces[tri++] = i1; faces[tri++] = 0;
        }
        Geometry geom = new Geometry()
        {
            vertices = vertices,
            normals = normals,
            vertexColors = colors,
            faces = faces
        };
        return geom;
    }

    List<Geometry> CombineGeometry(List<Geometry> geoms, int maxVertices = 65535)
    {
        int combinedIndex = 0;
        List<Geometry> combined = new List<Geometry>();
        if (geoms.Count == 1)
        {
            return geoms;
        }
        while (combinedIndex < geoms.Count)
        {
            int vertices_to_combine = 0;
            int faces_to_combine = 0;
            int mesh_to_combine = 0;
            for (mesh_to_combine = combinedIndex; mesh_to_combine < geoms.Count; mesh_to_combine++)
            {
                Geometry geom = geoms[mesh_to_combine];
                if (vertices_to_combine + geom.vertices.Length > maxVertices)
                {
                    break;
                }
                vertices_to_combine += geom.vertices.Length;
                faces_to_combine += geom.faces.Length;
            }
            if (mesh_to_combine == 1)
            {
                combined.Add(geoms[combinedIndex++]);
                continue;
            }
            Geometry newGeom = new Geometry()
            {
                vertices = new Vector3[vertices_to_combine],
                normals = new Vector3[vertices_to_combine],
                vertexColors = new Color32[vertices_to_combine],
                faces = new int[faces_to_combine]
            };
            int tri = 0;
            int vert = 0;
            for (int i = combinedIndex; i < mesh_to_combine; i++)
            {
                Geometry geom = geoms[i];
                geom.vertices.CopyTo(newGeom.vertices, vert);
                geom.normals.CopyTo(newGeom.normals, vert);
                geom.vertexColors.CopyTo(newGeom.vertexColors, vert);
                geom.faces.CopyTo(newGeom.faces, tri);
                if (vert != 0)
                {
                    for (int j = 0; j < geom.faces.Length; j++)
                    {
                        newGeom.faces[tri + j] += vert;
                    }
                }
                vert += geom.vertices.Length;
                tri += geom.faces.Length;

            }
            combinedIndex += mesh_to_combine;
            combined.Add(newGeom);
        }
        return combined;
    }
}
