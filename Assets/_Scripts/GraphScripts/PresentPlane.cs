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
		float step = (vector12.magnitude * vector23.magnitude * vector13.magnitude) / (2 * Vector3.Cross(vector12, vector23).magnitude);
		float pt1Coef = (Mathf.Pow(vector23.magnitude,2) * Vector3.Dot(vector12, vector13)) / (2 * Vector3.Cross(vector12, vector23).sqrMagnitude);
		float pt2Coef = (Mathf.Pow(vector13.magnitude,2) * Vector3.Dot((-1) * vector12, vector23)) / (2 * Vector3.Cross(vector12, vector23).sqrMagnitude);
		float pt3Coef = (Mathf.Pow(vector12.magnitude,2) * Vector3.Dot((-1) * vector13, (-1) * vector23)) / (2 * Vector3.Cross(vector12, vector23).sqrMagnitude);
		float centerX = pt1Coef * rawPt1.X.Value + pt2Coef * rawPt2.X.Value + pt3Coef * rawPt3.X.Value;
		float centerY = pt1Coef * rawPt1.Y.Value + pt2Coef * rawPt2.Y.Value + pt3Coef * rawPt3.Y.Value;
		float centerZ = pt1Coef * rawPt1.Z.Value + pt2Coef * rawPt2.Z.Value + pt3Coef * rawPt3.Z.Value;
		PtCoord centerPt = new PtCoord(new AxisCoord(centerX), new AxisCoord(centerY), new AxisCoord(centerZ));
		//TODO: find midpoint of triangle
	}
	public Vector3 generateVector(PtCoord pt1, PtCoord pt2)
	{
		Vector3 result = Vector3.zero;
		result.x = pt1.X.Value - pt2.X.Value;
		result.y = pt1.Y.Value - pt2.Y.Value;
		result.z = pt1.Z.Value - pt2.Z.Value;
		return result;
	}

	// Return the raw string of the equation
	public string calculatePlane() {
		vector12 = generateVector(rawPt1, rawPt2);
		vector13 = generateVector(rawPt1, rawPt3);
		normalVector = Vector3.Cross(vector12, vector13);
		// Basic formula of the equation
		rawEquation = rawPt1.X.Value + "* ( x -" + normalVector.x + ") + " +  
			rawPt1.Y.Value + "* ( y -" + normalVector.y + ") + " +  rawPt1.Z.Value + "* ( x -" + normalVector.z + ") = 0";

		return rawEquation;
	}
}
