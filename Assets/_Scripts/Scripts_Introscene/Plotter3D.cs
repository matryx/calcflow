using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class Plotter3D : MonoBehaviour {

    private int width = 0;
    private int height = 0;
    private float minX = 0f, maxX = 5f, minZ = 0f, maxZ = 5f;
    private float offset = 0.2f;

    private List<Vector3> vertices;
    private List<Vector2> uVs;
    private List<int> faces;

	private Vector3 topLeft;
	private Vector3 topRight;
	private Vector3 bottomLeft;
	private Vector3 bottomRight;

	public Material frameMaterial;

    public delegate float TwoVariableFunction(float x, float z);

	// Use this for initialization
	void Start () {
	}
	
	// Update is called once per frame
	void Update () {
	}

    public void Sampling3D(TwoVariableFunction myFn, float x_min, float x_max, float z_min, float z_max, float offset0)
    {
        width = height = 0;
        minX = x_min;maxX = x_max;minZ = z_min;maxZ = z_max;
        offset = offset0;

        vertices = new List<Vector3>();
        uVs = new List<Vector2>();
        faces = new List<int>();

		topLeft = new Vector3 (maxX, myFn (maxX, minZ), minZ);
		topRight = new Vector3 (maxX, myFn (maxX, maxZ), maxZ);
		bottomLeft = new Vector3 (minX, myFn (minX, minZ), minZ);
		bottomRight = new Vector3 (minX, myFn (minX, maxZ), maxZ);


        int total = 0;
		for(float i = minX; i <= maxX; i += offset)
        {
			for(float j = minZ; j <= maxZ; j += offset)
            {
				Vector3 point = new Vector3 (i, myFn (i, j), j);
				vertices.Add (point);

                total++;
            }
            width++;
        }
        height = total / width;
    }

    public void GenerateFaceData()
    {
        int num_quads = (width - 1) * (height - 1);
        int num_triangles = num_quads * 2;

        int vert = 0;
        for(int i = 0; i < num_triangles * 3;)
        {
            if(!(vert%height == height - 1))
            {
                faces.Add(vert);
                faces.Add(vert + 1);
                faces.Add(vert + height);
                faces.Add(vert + 1);
                faces.Add(vert + height + 1);
                faces.Add(vert + height);
                i += 6;
            }
            vert++;
        }
    }

    void BackfaceDuplicate()
    {
        int num_vertices = vertices.Count;
        for(int i = 0; i < num_vertices; i++)
        {
            //duplicate all vertices
            vertices.Add(vertices[i]);
        }
        int num_triangles = faces.Count;
        for(int i = 0; i < num_triangles; i+=3)
        {
            //reversely save new triangles
            faces.Add(faces[i] + num_vertices);
            faces.Add(faces[i + 2] + num_vertices);
            faces.Add(faces[i + 1] + num_vertices);
        }
    }

    public void DrawMesh()
    {
        GenerateFaceData();
        BackfaceDuplicate();

        Mesh mesh = GetComponent<MeshFilter>().mesh;

        mesh.Clear();

        mesh.SetVertices(vertices);
        mesh.SetUVs(0, uVs);
        mesh.SetTriangles(faces, 0);
        mesh.RecalculateNormals();

		MeshCollider meshColl = gameObject.AddComponent (typeof(MeshCollider)) as MeshCollider;
		meshColl.sharedMesh = mesh;

		DrawFrame ();
    }

	void DrawFrame () {
		GameObject frameObj = transform.Find ("Frame").gameObject;

		LineRenderer frameLine = frameObj.AddComponent<LineRenderer>();

        frameLine.startWidth = 0.02f;
        frameLine.endWidth = 0.02f;
		frameLine.positionCount = 3;
		frameLine.material = frameMaterial;

		// Draws over the vertical parts of the frame twice
		// but uses only one gameobject and only one linerenderer
		/*frameLine.SetPosition (0, transform.TransformPoint(topLeft));
		frameLine.SetPosition (1, transform.TransformPoint(new Vector3 (maxX, 0.0f, minZ)));
		frameLine.SetPosition (2, transform.TransformPoint(new Vector3 (maxX, 0.0f, maxZ)));
		frameLine.SetPosition (3, transform.TransformPoint(topRight));
		frameLine.SetPosition (4, transform.TransformPoint(new Vector3 (maxX, 0.0f, maxZ)));
		frameLine.SetPosition (5, transform.TransformPoint(new Vector3 (minX, 0.0f, maxZ)));
		frameLine.SetPosition (6, transform.TransformPoint(bottomRight));
		frameLine.SetPosition (7, transform.TransformPoint(new Vector3 (minX, 0.0f, maxZ)));
		frameLine.SetPosition (8, transform.TransformPoint(new Vector3 (minX, 0.0f, minZ)));
		frameLine.SetPosition (9, transform.TransformPoint(bottomLeft));
		frameLine.SetPosition (10, transform.TransformPoint(new Vector3 (minX, 0.0f, minZ)));
		frameLine.SetPosition (11, transform.TransformPoint(new Vector3 (maxX, 0.0f, minZ)));
		frameLine.SetPosition (12, transform.TransformPoint(topLeft));*/

		frameLine.SetPosition(0, transform.TransformPoint(new Vector3(0,0,0)));
		frameLine.SetPosition(1, transform.TransformPoint(new Vector3(1,1,0)));
		frameLine.SetPosition(2, transform.TransformPoint(new Vector3(2,2,3)));
	}

    public void ClearMesh()
    {
        Mesh mesh = GetComponent<MeshFilter>().mesh;
        mesh.Clear();
    }

    public void Delete()
    {
        Destroy(gameObject);
    }
}