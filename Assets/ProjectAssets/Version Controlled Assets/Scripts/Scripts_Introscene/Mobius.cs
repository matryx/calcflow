using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class Mobius : MonoBehaviour {
    private List<Vector3> path;
    private List<Vector3> normals;
    private int currIndex = 0;
    //private GameObject point;
    private LineRenderer normalLine;
    private int faceSide  = 1;

    public GameObject point;
    LineRenderer top;
    LineRenderer body;

	// Use this for initialization
	void Awake () {
        //point = GameObject.CreatePrimitive(PrimitiveType.Sphere);
        //point.name = "Mobius Point";
        //point.transform.SetParent(transform, false);
        initPath();
        point.transform.localPosition = path[currIndex];
    }

    void Start()
    {
        //Line Renderer Shit
        //normalLine = point.AddComponent<LineRenderer>();
        //normalLine.SetVertexCount(2);
        //normalLine.SetWidth(.01f, .01f);
        //normalLine.useWorldSpace = false;
        top = point.transform.Find("Top").GetComponent<LineRenderer>();
        body = point.transform.Find("Body").GetComponent<LineRenderer>();
    }

    void initPath()
    {
        Mesh mesh;
        path = new List<Vector3>();
        normals = new List<Vector3>();
        mesh = GetComponent<MeshFilter>().mesh;
        Vector3[] verts = mesh.vertices;
        /*
        for (int i = 0; i < verts.Length - 1; i++)
        {
            Debug.Log("old: " + verts[i]);
            verts[i] = transform.TransformPoint(verts[i]);
            Debug.Log("new: " + verts[i]);
        }
        */
        //int start = verts.Length / 2;
        for (int i = 0; i < verts.Length / 2; i++)
        {
            int curr = i + verts.Length / 2;
            int pre = curr - 1;
            int next = (curr != verts.Length - 1) ? curr + 1 : verts.Length / 2;
            float dist = (verts[curr] - verts[pre]).magnitude;
            Vector3 mid = (verts[curr] + verts[pre]) / 2;
            if (dist > 8.0)
            {
                Vector3 norm = Vector3.Cross(verts[curr] - verts[next], verts[curr] - verts[pre]);
                if ((normals.Count > 1) && (norm - normals[normals.Count - 1]).magnitude > 100)
                {
                    norm = -norm;
                }
                normals.Add(Vector3.Normalize(norm)*100f);
                path.Add(mid);
            }
        }
    }

	void Update () {

        if(Input.GetKeyDown(KeyCode.Space))
        {
            print(currIndex);
        }
	}

    public void dragPoint(Vector3 dragLoc)
    {
        int length = path.Count;
        Vector3 currPoint = path[currIndex];
        currPoint = transform.TransformPoint(currPoint);
        Vector3 prevPoint = path[(currIndex - 1 + length) % length];
        prevPoint = transform.TransformPoint(prevPoint);
        Vector3 nextPoint = path[(currIndex + 1) % length];
        nextPoint = transform.TransformPoint(nextPoint);

        if ((currPoint - dragLoc).magnitude > (prevPoint - dragLoc).magnitude)
        {
            currIndex = (currIndex - 1 + length) % length;
             if (currIndex == 136) faceSide = -faceSide;
            point.transform.position = prevPoint;
            //normalLine.SetPosition(0, new Vector3(0, 0, 0));
            //normalLine.SetPosition(1, normals[currIndex] * faceSide);
            body.SetPosition(0, new Vector3(0, 0, 0));
            body.SetPosition(1, (normals[currIndex] * faceSide).normalized * 1.5f);
            top.SetPosition(0, (normals[currIndex] * faceSide).normalized * 1.5f);
            top.SetPosition(1, (normals[currIndex] * faceSide).normalized * 3f);
            return;
        }
        if ((currPoint - dragLoc).magnitude > (nextPoint - dragLoc).magnitude)
        {
            currIndex = (currIndex + 1) % length;
            if (currIndex == 0) faceSide = -faceSide;
            point.transform.position = nextPoint;
            body.SetPosition(0, new Vector3(0, 0, 0));
            body.SetPosition(1, (normals[currIndex] * faceSide).normalized * 1.5f);
            top.SetPosition(0, (normals[currIndex] * faceSide).normalized * 1.5f);
            top.SetPosition(1, (normals[currIndex] * faceSide).normalized * 3f);
            return;
        }
    }
}
