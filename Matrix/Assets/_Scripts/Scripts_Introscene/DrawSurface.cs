using UnityEngine;
using System.Collections;
using System.Linq;
using System.Collections.Generic;

public class DrawSurface : MonoBehaviour {

	public Plotter3D surface;

	public float minX, maxX, minZ, maxZ;
	private float offset = 0.2f;

	// Use this for initialization
	void Start () {
		Plotter3D s = (Plotter3D)Instantiate (surface, Vector3.zero, Quaternion.identity);
        s.transform.SetParent(transform.parent, false);
		s.Sampling3D (myFn, minX, maxX, minZ, maxZ, offset);
		s.DrawMesh ();
	}
	
	float myFn(float x, float z) {
		//return Mathf.Pow (Mathf.Cos (x), 2f) + Mathf.Pow (Mathf.Cos (z), 2f) + 3.0f;
		return .5f * Mathf.Cos(x) + .5f * Mathf.Cos(z) + 1;
	}
}