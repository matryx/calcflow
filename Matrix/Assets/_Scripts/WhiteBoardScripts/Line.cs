using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class Line : MonoBehaviour {
	private Material material;
	private LineRenderer lr;
	private Vector3 end;
	private List<Vector3> points = new List<Vector3>();
	private int numPoints = 1;
	private float width = .005f;

	public static Line createLine(){
		return new GameObject("Line").AddComponent<Line>();
	}

    public void startLine(Vector3 s) {
        lr = gameObject.AddComponent<LineRenderer>();
        lr.positionCount = numPoints;
        lr.useWorldSpace = false;
        lr.startWidth = width;
        lr.endWidth = width;
        lr.material = material;
        lr.SetPosition(0, Vector3.zero);
        points.Add(Vector3.zero);
        lr.transform.position = s;
    }

    public void addPoint(Vector3 pos) {
		numPoints++;
		//points.Add (pos-transform.position);
		points.Add (transform.InverseTransformPoint(pos));
        lr.positionCount = numPoints;
		lr.useWorldSpace = false;
        lr.startWidth = width;
        lr.endWidth = width;
        lr.material = material;
		lr.SetPositions (points.ToArray());
	}

	public void setWidth(float penWidth){
		width = penWidth;
	}

	public void setMaterial (Material mat) {
		material = mat;
	}

	//customization methods
	public void setColor() {

	}

	public Vector3 getPoint(int i) {
		return transform.TransformPoint(points[i]);
	}

	public int getNumPoints(){
		return numPoints;
	}
}