using UnityEngine;
using System;
using System.Collections;

public class ClosedBoundryRenderer : MonoBehaviour {

    MeshFilter meshFilter;
    Mesh mesh;

    float a; // x+y+z=a (a>0)

    public ConstraintGrabbable slider;

	// Use this for initialization
	void Start () {
        a = 5f;

        meshFilter = GetComponent<MeshFilter>();
        mesh = meshFilter.mesh;

        GenerateMesh();
	}
	
	// Update is called once per frame
	void Update () {
        slider.transform.Find("Equation").GetComponent<TextMesh>().text = "x+y+z=" + String.Format("{0:F3}", slider.lastLocalPos.z);

        UpdateA(slider.lastLocalPos.z);
        GenerateMesh();    
	}

    void UpdateA(float a0)
    {
        a = a0;
    }

    void GenerateMesh()
    { 
        Vector3[] vertices =
        {
            new Vector3(0f,0f,0f),
            new Vector3(a,0f,0f),
            new Vector3(0f,a,0f),
            new Vector3(0f,0f,a)
        };

        int[] triangles =
        {
            0,2,1,
            0,1,3,
            0,3,2,
            1,2,3
        };

        mesh.vertices = vertices;
        mesh.triangles = triangles;
        mesh.RecalculateNormals();
    }
}
