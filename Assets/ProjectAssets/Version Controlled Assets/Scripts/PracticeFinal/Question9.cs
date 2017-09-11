using UnityEngine;
using System;
using System.Collections;
using AK;
using System.IO;
using System.Threading;
using System.Collections.Generic;

public class Question9 : CustomParametrizedSurface {

	public Transform vPrefab;
    List<Transform> vectors;
	List<Vector3> startPts;
	List<Vector3> offsets;
	//float max_magnitude = 1.0f;

    // Use this for initialization
    protected override void Start () {
        base.Start();

        vectors = new List<Transform>();
		startPts = new List<Vector3>();
		offsets = new List<Vector3>();
		//max_magnitude = 0f;

        GenerateVectors();
        DrawVectorField();
	}

    public void GenerateVectors()
    {
        var radius = 3;
        for (float currY = -2.5f; currY < 3; currY += 1f)
        {
            for (int k = 0; k < 18; k++)
            {
                float theta = (float)k / 9f * Mathf.PI;
                float currR = Mathf.Sqrt(radius * radius - currY * currY);
                float currX = currR * Mathf.Cos(theta);
                float currZ = currR * Mathf.Sin(theta);
                Vector3 startPt = new Vector3(currX, currY, currZ);
                Vector3 normal = transform.TransformDirection(startPt.normalized);
                startPts.Add(startPt);
                Vector3 result = normal;
                offsets.Add(result);
            }
        }
    }

    void DrawVectorField()
	{
		for (int i = 0; i < startPts.Count; i++)
		{
			Vector3 target = startPts[i];
			Vector3 offset = offsets[i];
			Vector3 tip = offset * 0.4f;

			Transform l = Instantiate(vPrefab);
			l.SetParent(transform.Find("NormalVectors"), false);
			LineRenderer top = l.Find("Top").GetComponent<LineRenderer>();
			top.SetPosition(0, target + offset - tip);
			top.SetPosition(1, target + offset);
			LineRenderer body = l.Find("Body").GetComponent<LineRenderer>();
			body.SetPosition(0, target + offset - tip);
			body.SetPosition(1, target);
			vectors.Add(l);

			Color c = gradient.Evaluate(offset.magnitude);
			top.material.color = c;
            body.material.color = c;
        }
    }

	void Clear()
	{
		foreach (Transform t in vectors)
		{
			Destroy(t.gameObject);
		}
		vectors.Clear();
		startPts.Clear();
		offsets.Clear();
	}

	public void UpdateFunctions()
	{
		Clear();
		DrawVectorField();
	}
}
