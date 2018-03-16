using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PresentPlane : MonoBehaviour {

	public Transform point1;
	public Transform point2;
	public Transform point3;

	public Transform plane;
	public Transform forwardPlane;
	public Transform backwardPlane;
	public Transform lookAtTarget;
	public List<GameObject> walls;

	public PtManager ptManager;
	private PtCoord rawPt1;
	private PtCoord rawPt2;
	private PtCoord rawPt3;

	Vector3 center;
	

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
		float stepSize = (vector12.magnitude * vector23.magnitude * vector13.magnitude) / (2 * Vector3.Cross(vector12, vector23).magnitude);
		float pt1Coef = (Mathf.Pow(vector23.magnitude,2) * Vector3.Dot(vector12, vector13)) / (2 * Vector3.Cross(vector12, vector23).sqrMagnitude);
		float pt2Coef = (Mathf.Pow(vector13.magnitude,2) * Vector3.Dot((-1) * vector12, vector23)) / (2 * Vector3.Cross(vector12, vector23).sqrMagnitude);
		float pt3Coef = (Mathf.Pow(vector12.magnitude,2) * Vector3.Dot((-1) * vector13, (-1) * vector23)) / (2 * Vector3.Cross(vector12, vector23).sqrMagnitude);
		float centerX = pt1Coef * rawPt1.X.Value + pt2Coef * rawPt2.X.Value + pt3Coef * rawPt3.X.Value;
		float centerY = pt1Coef * rawPt1.Y.Value + pt2Coef * rawPt2.Y.Value + pt3Coef * rawPt3.Y.Value;
		float centerZ = pt1Coef * rawPt1.Z.Value + pt2Coef * rawPt2.Z.Value + pt3Coef * rawPt3.Z.Value;
		center = new Vector3(centerX, centerY, centerZ);
		PtCoord centerPt = new PtCoord(new AxisCoord(centerX), new AxisCoord(centerY), new AxisCoord(centerZ));
		//Get the range of the box
		xLabelManager.Min = centerPt.X.Value - stepSize * steps;
		yLabelManager.Min = centerPt.Y.Value - stepSize * steps;
		zLabelManager.Min = centerPt.Z.Value - stepSize * steps;
		xLabelManager.Max = centerPt.X.Value + stepSize * steps;
		yLabelManager.Max = centerPt.Y.Value + stepSize * steps;
		zLabelManager.Max = centerPt.Z.Value + stepSize * steps;
		//Get the interaction points between the box edges and the plane
		expr = solver.SymbolicateExpression(rawEquation);
		getVerticesList();
	}

	public Vector3 GenerateVector(PtCoord pt1, PtCoord pt2)
	{
		Vector3 result = Vector3.zero;
		result.x = pt1.X.Value - pt2.X.Value;
		result.y = pt1.Y.Value - pt2.Y.Value;
		result.z = pt1.Z.Value - pt2.Z.Value;
		return result;
	}

	// Return the raw string of the equation
	public string CalculatePlane() 
	{
		vector12 = GenerateVector(rawPt1, rawPt2);
		vector13 = GenerateVector(rawPt1, rawPt3);
		if (PlaneValid()) {
			normalVector = Vector3.Cross(vector12, vector13);

			
			//plane.rotation = Quaternion.Euler(normalVector);
			
			//plane.rotation = Quaternion.FromToRotation(Vector3.zero, normalVector-center);
			plane.LookAt(lookAtTarget);
			print(Quaternion.FromToRotation(Vector3.zero, normalVector-center));
			
			// Basic formula of the equation

			d = rawPt1.X.Value * normalVector.x + rawPt1.Y.Value * normalVector.y + rawPt1.Z.Value * normalVector.z;
			rawEquation = normalVector.x + "x+" + normalVector.y + "y+" + normalVector.z + "z=" + d;
		}


		return rawEquation;
	}

	public bool PlaneValid() {
		bool result = true;
		
		if (vector12.Equals(Vector3.zero) || vector13.Equals(Vector3.zero)) {
			result = false;
		}
		
		Vector3 temp = vector13 * (vector12.x / vector13.x);
		if (vector12.Equals(temp)) {
			result = false;
		}

		return result;
	}

	public Vector3 CalculateX(float y, float z)
	{
		return new Vector3((- normalVector.y * y - normalVector.z * z + d) / normalVector.x, y, z);
	}

	public Vector3 CalculateY(float x, float z)
	{
		return new Vector3(x, (- normalVector.x * x - normalVector.z * z + d) / normalVector.y, z);
	}

	public Vector3 CalculateZ(float x, float y)
	{
		return new Vector3(x, y, (- normalVector.y * y - normalVector.x * x + d) / normalVector.z);
	}

	public bool PointWithinBox(float val, float min, float max)
	{
		return (val >= min) && (val <= max);
	}

	public void getVerticesList() 
	{
		List<Vector3> xVertices = new List<Vector3>();
		xVertices.Add(CalculateX(yLabelManager.Min, zLabelManager.Min));
		xVertices.Add(CalculateX(yLabelManager.Min, zLabelManager.Max));
		xVertices.Add(CalculateX(yLabelManager.Max, zLabelManager.Max));
		xVertices.Add(CalculateX(yLabelManager.Max, zLabelManager.Min));
		xVertices.RemoveAll(point => !PointWithinBox(point.x, xLabelManager.Min, xLabelManager.Max));
		List<Vector3> yVertices = new List<Vector3>();
		yVertices.Add(CalculateY(xLabelManager.Min, zLabelManager.Min));
		yVertices.Add(CalculateY(xLabelManager.Min, zLabelManager.Max));
		yVertices.Add(CalculateY(xLabelManager.Max, zLabelManager.Max));
		yVertices.Add(CalculateY(xLabelManager.Max, zLabelManager.Min));
		yVertices.RemoveAll(point => !PointWithinBox(point.y, yLabelManager.Min, yLabelManager.Max));
		List<Vector3> zVertices = new List<Vector3>();
		zVertices.Add(CalculateZ(xLabelManager.Min, yLabelManager.Min));
		zVertices.Add(CalculateZ(xLabelManager.Min, yLabelManager.Max));
		zVertices.Add(CalculateZ(xLabelManager.Max, yLabelManager.Max));
		zVertices.Add(CalculateZ(xLabelManager.Max, yLabelManager.Min));
		zVertices.RemoveAll(point => !PointWithinBox(point.z, zLabelManager.Min, zLabelManager.Max));
		
	}
}
