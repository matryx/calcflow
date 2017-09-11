using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using MathFunctions;

public class SphereMappedVectorField : MonoBehaviour {

    public enum MappingOptions { SPHERE, TORUS};
    public MappingOptions mappingOption;
    private MappingOptions currMapping;

    public Transform vPrefab;

    private List<Vector3> startPts;
    private List<Vector3> results;
    private List<Vector3> normals;
    private List<Vector3> tangents;

    private List<Transform> vectors;

    public VectorFieldFunctions.VecFieldFnOptions sampleFunction;
    private VectorFieldFunctions.VecFieldFnOptions currFunction;

    Vector3 lastPosition;
    Quaternion lastRotation;

    // Use this for initialization
    void Start () {
        startPts = new List<Vector3>();
        results = new List<Vector3>();
        tangents = new List<Vector3>();

        vectors = new List<Transform>();

        //SampleSphere (VectorFieldFunctions.vecFieldFns[(int)sampleFunction], 5f);
        if (mappingOption == MappingOptions.SPHERE)
        {
            SampleSphere(VectorFieldFunctions.vecFieldFns[(int)sampleFunction], 5f);
        }
        else {
            SampleTorus(VectorFieldFunctions.vecFieldFns[(int)sampleFunction], 6f, 1.5f);
        }
        DrawVectorField();

        lastPosition = transform.position;
        lastRotation = transform.rotation;
	}
	
	// Update is called once per frame
	void Update () {
        if(currFunction != sampleFunction || currMapping != mappingOption
            || lastPosition != transform.position || lastRotation != transform.rotation)
        {
            Clear();
            if (mappingOption == MappingOptions.SPHERE)
            {
                SampleSphere(VectorFieldFunctions.vecFieldFns[(int)sampleFunction], 5f);
            }
            else {
                SampleTorus(VectorFieldFunctions.vecFieldFns[(int)sampleFunction], 6f, 1.5f);
            }
            DrawVectorField();
        }
        lastPosition = transform.position;
        lastRotation = transform.rotation;
	}

    public void SampleSphere(VectorFieldFunctions.VectorFieldFunction myFn, float radius)
    {
        currFunction = sampleFunction;
        currMapping = mappingOption;

        float y = -radius + 0.5f;
        Vector3 temp = new Vector3(0, -radius, 0);
        temp = transform.TransformPoint(temp);
        startPts.Add(temp);
        results.Add(myFn(temp));
        tangents.Add(Vector3.Cross(temp, myFn(temp)).normalized * 0.5f);

        temp = new Vector3(0, radius, 0);
        temp = transform.TransformPoint(temp);
        startPts.Add(temp);
        results.Add(myFn(temp));
        tangents.Add(Vector3.Cross(temp, myFn(temp)).normalized * 0.5f);

        for (float currY = y; currY < radius; currY+=0.5f)
        {
            for(int i = 0; i < 36; i++)
            {
                float theta = (float)i / 18f * Mathf.PI;
                float currR = Mathf.Sqrt(radius * radius - currY * currY);
                float currX = currR * Mathf.Cos(theta);
                float currZ = currR * Mathf.Sin(theta);
                Vector3 startPt = new Vector3(currX, currY, currZ);
                Vector3 normal = transform.TransformDirection(startPt.normalized);
                startPt = transform.TransformPoint(startPt);
                startPts.Add(startPt);
                Vector3 result = myFn(startPt);
                results.Add(result);
                tangents.Add(Vector3.Cross(normal.normalized, result.normalized).normalized * 0.5f);
            }
        }
    }

    public void DrawVectorField()
    {
        for (int i = 0; i < startPts.Count; i++)
        {
            Vector3 target = startPts[i];
            Vector3 offset = tangents[i];
            Vector3 tip = offset * 0.4f;

            Transform l = Instantiate(vPrefab);
            l.SetParent(transform, false);
            LineRenderer top = l.Find("Top").GetComponent<LineRenderer>();
            top.SetPosition(0, transform.InverseTransformPoint(target + offset - tip));
            top.SetPosition(1, transform.InverseTransformPoint(target + offset));
            LineRenderer body = l.Find("Body").GetComponent<LineRenderer>();
            body.SetPosition(0, transform.InverseTransformPoint(target + offset - tip));
            body.SetPosition(1, transform.InverseTransformPoint(target));
            vectors.Add(l);
        }
    }

    void ClearData()
    {
        startPts.Clear();
        results.Clear();
        tangents.Clear();
    }

    void Clear()
    {
        foreach (Transform t in vectors)
        {
            Destroy(t.gameObject);
        }
        vectors.Clear();
        startPts.Clear();
        results.Clear();
        tangents.Clear();
    }

    public void SampleTorus(VectorFieldFunctions.VectorFieldFunction myFn, float radius_torus, float radius_cross)
    {
        currFunction = sampleFunction;
        currMapping = mappingOption;

        for (int i = 0; i < 36; i++)
        {
            float theta = (float)i / 18f * Mathf.PI;
            float x_center = Mathf.Cos(theta) * radius_torus;
            float z_center = Mathf.Sin(theta) * radius_torus;
            float y_center = 0f;
            for(int j = 0; j < 12; j++)
            {
                float omega = (float)j / 6f * Mathf.PI;
                float y = Mathf.Sin(omega) * radius_cross;
                float x = Mathf.Cos(theta) * (Mathf.Cos(omega) * radius_cross+radius_torus);
                float z = Mathf.Sin(theta) * (Mathf.Cos(omega) * radius_cross+radius_torus);
                Vector3 startPt = new Vector3(x, y, z);
                startPt = transform.TransformPoint(startPt);
                Vector3 result = myFn(startPt);
                Vector3 normal = transform.InverseTransformPoint(startPt) - new Vector3(x_center, y_center, z_center);
                normal = normal.normalized;
                startPts.Add(startPt);
                results.Add(result);
                tangents.Add(Vector3.Cross(transform.TransformDirection(normal), result).normalized * 0.5f);
            }
        }
        
        /*
        for(int i = 0; i < 36; i++)
        {
            float u = (float)i / 18f * Mathf.PI;
            for(int j = 0; j < 36; j++)
            {
                float v = (float)i / 18f * Mathf.PI;
                float x = (radius_torus + radius_cross * Mathf.Cos(v)) * Mathf.Cos(u);
                float y = (radius_torus + radius_cross * Mathf.Cos(v)) * Mathf.Sin(u);
            }
        }
        */
    }
}
