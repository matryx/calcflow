using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PresentPlane : MonoBehaviour {

	public Transform point1;
	public Transform point2;
	public Transform point3;

	public PtManager ptManager;
	public PtCoord rawPt1;
	public PtCoord rawPt2;
	public PtCoord rawPt3;

	public Vector3 vector12;
	public Vector3 vector13;
	public Vector3 vector23;
	public Vector3 normalVector;

	public string rawEquation;

	AK.ExpressionSolver solver;

    public AxisLabelManager xLabelManager;
    public AxisLabelManager yLabelManager;
	public AxisLabelManager zLabelManager;

	void Awake()
	{
		solver = new AK.ExpressionSolver();
		if (pointReady())
		{
			rawPt1 = ptManager.ptSet.ptCoords["pt1"];
			rawPt2 = ptManager.ptSet.ptCoords["pt2"];
			rawPt3 = ptManager.ptSet.ptCoords["pt3"];

			ApplyGraphAdjustment();

		}
	}

	public bool pointReady()
	{
		return ptManager != null && ptManager.ptSet != null;
	}

	public void ApplyGraphAdjustment()
	{
		vector23 = generateVector(rawPt2, rawPt3);
		//TODO: find midpoint of triangle
	}

	public float generateModule (Vector3 myVector) 
	{
		return Mathf.Sqrt(myVector.x*myVector.x + myVector.y*myVector.y + myVector.z*myVector.z);
	}
	// return vector from pt1 to pt2
	public Vector3 generateVector(PtCoord pt1, PtCoord pt2)
	{
		Vector3 result = Vector3.zero;
		result.x = pt2.X.Value - pt1.X.Value;
		result.y = pt2.Y.Value - pt1.Y.Value;
		result.z = pt2.Z.Value - pt1.Z.Value;
		return result;
	}

	// Return the raw string of the equation
	public string  calculatePlane() {
		vector12 = generateVector(rawPt1, rawPt2);
		vector13 = generateVector(rawPt1, rawPt3);
		// Calculate the normal vector
		normalVector.x = vector12.y * vector13.z - vector12.z * vector13.y;
		normalVector.y = -(vector12.x * vector13.z - vector12.z * vector13.x);
		normalVector.z = vector12.x * vector13.y - vector12.y * vector13.x;
		
		// Basic formula of the equation
		rawEquation = rawPt1.X.Value + "* ( x -" + normalVector.x + ") + " +  
			rawPt1.Y.Value + "* ( y -" + normalVector.y + ") + " +  rawPt1.Z.Value + "* ( x -" + normalVector.z + ") = 0";

		return rawEquation;
	}
}
