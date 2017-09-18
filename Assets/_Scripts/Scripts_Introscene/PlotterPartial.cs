using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class PlotterPartial : MonoBehaviour {

	float minX, minZ;
	float? maxX, maxZ;
	float offset;

	List<Vector3> curve;

	List<Vector3> vertices;
	List<Vector2> uVs;
	List<int> faces;

	//Plotter3D.TwoVariableFunction FN;

	// Use this for initialization
	void Start () {
	}
	
	// Update is called once per frame
	void Update () {
	}

	public void SampleCurve(Plotter3D.TwoVariableFunction myFn, float x_min, float? x_max, float z_min, float? z_max, float offset){
		minX = x_min;maxX = x_max;minZ = z_min;maxZ = z_max;
		//FN = myFn;

		curve = new List<Vector3> ();
		vertices = new List<Vector3>();
		uVs = new List<Vector2>();
		faces = new List<int>();

		if (maxX == null && maxZ == null) {
			Debug.Log ("ERROR");
			// error
		} else if (maxX == null) {
			for (float j = minZ; j <= maxZ; j += offset) {
				curve.Add(new Vector3(minX, myFn(minX, j), j));
			}
		} else if (maxZ == null) {
			for (float i = minX; i <= maxX; i += offset) {
				curve.Add (new Vector3 (i, myFn (i, minZ), minZ));
			}
		}
	}

	public void GenerateFaceData(float minY)
	{
		float tempY = -Mathf.Infinity;
		foreach (Vector3 point in curve) {
			if (point.y > tempY) {
				tempY = point.y;
			}
		}

		if (minY > tempY) {
			minY = tempY;
		}

		foreach (Vector3 point in curve) {
			vertices.Add (point);
			vertices.Add (new Vector3 (point.x, minY, point.z));
		}

		int num_quads = curve.Count;
		//int num_triangles = num_quads * 2;

		for (int i = 0; i < vertices.Count - 2; i += 2) {
			faces.Add (i);
			faces.Add (i + 2);
			faces.Add (i + 1);

			faces.Add (i + 1);
			faces.Add (i + 2);
			faces.Add (i + 3);
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

	public void DrawMesh(float y)
	{
		GenerateFaceData(y);
		BackfaceDuplicate();

		Mesh mesh = GetComponent<MeshFilter>().mesh;

		mesh.Clear();

		mesh.SetVertices(vertices);
		mesh.SetUVs(0, uVs);
		mesh.SetTriangles(faces, 0);
		mesh.RecalculateNormals();

	}

	public void ClearMesh()
	{
		Mesh mesh = GetComponent<MeshFilter>().mesh;
		mesh.Clear();
	}

	public void ClearData()
	{
		GetComponent<MeshFilter> ().mesh.Clear ();

		curve.Clear ();
		vertices.Clear ();uVs.Clear ();faces.Clear ();
		curve = vertices = null;
		uVs = null;
		faces = null;
	}

	public void Delete()
	{
		Destroy(gameObject);
	}
}