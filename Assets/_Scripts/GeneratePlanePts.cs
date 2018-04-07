using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class GeneratePlanePts : MonoBehaviour {

	public float a, b, c, d;
	public Vector3 rawCenter;
	public float dummyStep = 5;

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		rawCenter = getCenter(a,b,c,d);
	}

	public Vector3 getCenter(float a, float b, float c, float d) 
	{
		Vector3 normal = new Vector3(a,b,c); 
		float x = (-1)*a*d / (normal.sqrMagnitude);
		float y = (-1)*b*d / (normal.sqrMagnitude);
		float z = (-1)*c*d / (normal.sqrMagnitude);
		return new Vector3(x,y,z);
	}

	public Vector3[] getSidePoints(string face) 
	{
		Vector3 normal = new Vector3(rawCenter.x - dummyStep ,b,c);
		return new Vector3[2];
	}
}
