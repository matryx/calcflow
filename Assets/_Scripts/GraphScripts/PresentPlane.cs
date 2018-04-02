using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PresentPlane : MonoBehaviour {

	public Transform point1;
	public Transform point2;
	public Transform point3;
	public Transform centerPt;

	public Transform plane;
	public Transform forwardPlane;
	public Transform backwardPlane;
	public Transform lookAtTarget;
	public List<GameObject> walls;

	public PtManager ptManager;
	private PtCoord rawPt1;
	private PtCoord rawPt2;
	private PtCoord rawPt3;

	public Vector3 center;
	

	public PtSet ptSet;

	public Vector3 vector12;
	public Vector3 vector13;
	public Vector3 vector23;
	public Vector3 normalVector;
	public List<Vector3> vertices; 
	public string rawEquation;

	AK.ExpressionSolver solver;

	AK.Expression expr;

    public AxisLabelManager xLabelManager;
    public AxisLabelManager yLabelManager;
	public AxisLabelManager zLabelManager;

	public float steps = 3;
	public float dummySteps = 10;
	public float stepSize;
	public float defaultStepSize = 5;

	public float temp1;
	public float temp2;
	public float temp3;
	public float temp;
	public float temp0;

	public float d;

	void Awake()
	{
		solver = new AK.ExpressionSolver();
		expr = new AK.Expression();
		vertices = new List<Vector3>();
		if (pointReady())
		{
			rawPt1 = ptManager.ptSet.ptCoords["pt1"];
			rawPt2 = ptManager.ptSet.ptCoords["pt2"];
			rawPt3 = ptManager.ptSet.ptCoords["pt3"];
		}
	}

	public bool pointReady()
	{
		return ptManager != null && ptManager.ptSet != null;
	}

	void Update() {
		plane.LookAt(lookAtTarget);

		var sharedMaterial = forwardPlane.GetComponent<MeshRenderer>().sharedMaterial;
		sharedMaterial.SetInt("_planeClippingEnabled", 1);

		for (int i = 0; i < 6; i++) {
			sharedMaterial.SetVector("_planePos" + i, walls[i].transform.position);
			//plane normal vector is the rotated 'up' vector.
			sharedMaterial.SetVector("_planeNorm" + i, walls[i].transform.rotation * Vector3.up);
		}

		sharedMaterial = backwardPlane.GetComponent<MeshRenderer>().sharedMaterial;
		sharedMaterial.SetInt("_planeClippingEnabled", 1);

		for (int i = 0; i < 6; i++) {
			sharedMaterial.SetVector("_planePos" + i, walls[i].transform.position);
			//plane normal vector is the rotated 'up' vector.
			sharedMaterial.SetVector("_planeNorm" + i, walls[i].transform.rotation * Vector3.up);
		}
	}

	public void ApplyGraphAdjustment()
	{

		vector23 = GenerateVector(rawPt2, rawPt3);
		float pt1Coef = (Mathf.Pow(vector23.magnitude,2) * Vector3.Dot(vector12, vector13)) / (2 * Vector3.Cross(vector12, vector23).sqrMagnitude);
		float pt2Coef = (Mathf.Pow(vector13.magnitude,2) * Vector3.Dot( (-1) * vector12, vector23)) / (2 * Vector3.Cross(vector12, vector23).sqrMagnitude);
		float pt3Coef = (Mathf.Pow(vector12.magnitude,2) * Vector3.Dot(vector13, vector23)) / (2 * Vector3.Cross(vector12, vector23).sqrMagnitude);


		if (pt1Coef != pt1Coef || pt2Coef != pt2Coef || pt3Coef != pt3Coef) {
			center = (PtCoordToVector(rawPt1) + PtCoordToVector(rawPt2) + PtCoordToVector(rawPt3)) / 3;
			stepSize = Mathf.Max(vector12.magnitude, vector23.magnitude);
			if (stepSize == 0) {
				stepSize = defaultStepSize;
			}
		} else {
			stepSize = (vector12.magnitude * vector23.magnitude * vector13.magnitude) / (2 * Vector3.Cross(vector12, vector23).magnitude);
			float centerX = pt1Coef * rawPt1.X.Value + pt2Coef * rawPt2.X.Value + pt3Coef * rawPt3.X.Value;
			float centerY = pt1Coef * rawPt1.Y.Value + pt2Coef * rawPt2.Y.Value + pt3Coef * rawPt3.Y.Value;
			float centerZ = pt1Coef * rawPt1.Z.Value + pt2Coef * rawPt2.Z.Value + pt3Coef * rawPt3.Z.Value;
			temp1 = centerX;
			temp2 = centerY;
			temp3 = centerZ;
			center = new Vector3(centerX, centerY, centerZ);
		}
		//PtCoord centerPt = new PtCoord(new AxisCoord(centerX), new AxisCoord(centerY), new AxisCoord(centerZ));
		//Get the range of the box
		xLabelManager.Min = center.x - stepSize * steps;
		yLabelManager.Min = center.y - stepSize * steps;
		zLabelManager.Min = center.z - stepSize * steps;
		xLabelManager.Max = center.x + stepSize * steps;
		yLabelManager.Max = center.y + stepSize * steps;
		zLabelManager.Max = center.z + stepSize * steps;
		//Get the interaction points between the box edges and the plane
		//expr = solver.SymbolicateExpression(rawEquation);
	}

	public Vector3 GenerateVector(PtCoord pt1, PtCoord pt2)
	{
		Vector3 result = Vector3.zero;
		result.x = pt2.X.Value - pt1.X.Value;
		result.y = pt2.Y.Value - pt1.Y.Value;
		result.z = pt2.Z.Value - pt1.Z.Value;
		return result;
	}

	// Return the raw string of the equation
	public string CalculatePlane() 
	{
		vector12 = GenerateVector(rawPt1, rawPt2);
		vector13 = GenerateVector(rawPt1, rawPt3);
		normalVector = Vector3.Cross(vector12, vector13);
		if (PlaneValid()) {
			plane.LookAt(lookAtTarget);
			
			// Basic formula of the equation

			d = rawPt1.X.Value * normalVector.x + rawPt1.Y.Value * normalVector.y + rawPt1.Z.Value * normalVector.z;
			rawEquation = normalVector.x + "x+" + normalVector.y + "y+" + normalVector.z + "z=" + d;
		}

		return rawEquation;
	}

	public void GetLocalPoint() {
		point1.localPosition = ScaledPoint(PtCoordToVector(rawPt1));
		point2.localPosition = ScaledPoint(PtCoordToVector(rawPt2));
		point3.localPosition = ScaledPoint(PtCoordToVector(rawPt3));
		centerPt.localPosition = ScaledPoint(center);
	}

	public void GetPlaneDirection() {
		if (PlaneValid()) {
			float scale = dummySteps * stepSize / normalVector.magnitude;
			//Debug.Log("The scale is: " + scale + ". The Normal vectoer before scale is " + normalVector);
			Vector3 dummyPos = normalVector * scale;
			//Debug.Log("The Normal vector after scale is: " + dummyPos);
			lookAtTarget.localPosition = ScaledPoint(dummyPos);
			plane.localPosition = ScaledPoint(center);
		}
	}

	public bool PlaneValid() {		
		if (PtCoordToVector(rawPt1) == PtCoordToVector(rawPt2) || PtCoordToVector(rawPt1) == PtCoordToVector(rawPt3) || PtCoordToVector(rawPt2) == PtCoordToVector(rawPt3)) {
			return false; 
		}
		
		float scale;
		if (vector13.x != 0) {
			scale = vector12.x / vector13.x;
		} else if (vector13.y != 0) {
			scale = vector12.y / vector13.y;
		} else {
			scale = vector12.z / vector13.z;
		}
		Vector3 temp = vector13 * scale;
		if (vector12.Equals(temp)) {
			return false;
		}

		return true;
	}

	public Vector3 PtCoordToVector(PtCoord pt) {
		return (new Vector3(pt.X.Value, pt.Y.Value, pt.Z.Value));
	}

	public Vector3 ScaledPoint(Vector3 pt) {
		Vector3 result = Vector3.zero;
		print("raw pt1 position: " + pt);
		result.z = (pt.x - xLabelManager.Min) / (xLabelManager.Max - xLabelManager.Min) * 20 - 10;
		result.x = (pt.y - yLabelManager.Min) / (yLabelManager.Max - yLabelManager.Min) * 20 - 10;
		result.y = (pt.z - zLabelManager.Min) / (zLabelManager.Max - zLabelManager.Min) * 20 - 10;
		return result;
	}
}
