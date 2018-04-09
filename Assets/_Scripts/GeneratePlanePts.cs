using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class GeneratePlanePts : MonoBehaviour {

	public float a, b, c, d;
	public Vector3 pt1;
	public Vector3 pt2;
	public Vector3 pt3;
	public PresentPlane myPlane;
	public float steps = 3;
	public float stepSize = 5;
	public AxisLabelManager xLabelManager;
    public AxisLabelManager yLabelManager;
	public AxisLabelManager zLabelManager;
	public Vector3 center;

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		getCenter();
		changeScale();
		bool check = getSidePoints(null, center.y, center.z);
		if (check == false)
		{
			check = getSidePoints(center.x, null, center.z);
		}
		if (check == false)
		{
			check = getSidePoints(center.x, center.y, null);
		}

	}

	public void getCenter() 
	{
		Vector3 normal = new Vector3(a,b,c); 
		center = Vector3.ProjectOnPlane(Vector3.zero, normal);
	}

	public void changeScale() 
	{
		xLabelManager.Min = center.x - stepSize * steps;
		yLabelManager.Min = center.y - stepSize * steps;
		zLabelManager.Min = center.z - stepSize * steps;
		xLabelManager.Max = center.x + stepSize * steps;
		yLabelManager.Max = center.y + stepSize * steps;
		zLabelManager.Max = center.z + stepSize * steps;
	}

	public bool getSidePoints(float? x, float? y, float? z) 
	{
		if(x == null)
		{
			if (a == 0)
			{
				return false;
			}
			return checkPtValid((float)y, (float)x, (float)z, xLabelManager); 
		}
		else if (y == null)
		{
			if (b == 0)
			{
				return false;
			}
			return checkPtValid((float)x, (float)y, (float)z, yLabelManager);
		}
		else
		{
			if (c == 0)
			{
				return false;
			}
			return checkPtValid((float)x, (float)z, (float)y, zLabelManager);
		}
	}

	public bool checkPtValid(float width, float depth, float height, AxisLabelManager myAxis)
	{
			float newWidth = width-steps; 
			float newHeight = height-steps;
			float newDepth = (d - a*newWidth - c*newHeight)/b;
			if(newDepth > myAxis.Max || newDepth < myAxis.Min)
			{
				return false;
			}
			Vector3 temp1 = new Vector3(newWidth, newHeight, newDepth);

			newWidth = width-steps; 
			newHeight = height+steps;
			newDepth = (d - a*newWidth - c*newHeight)/b;
			if(newDepth > myAxis.Max || newDepth < myAxis.Min)
			{
				return false;
			}
			Vector3 temp2 = new Vector3(newWidth, newHeight, newDepth);

			newWidth = width-steps;
			newDepth = (d - a*newWidth - c*height)/b;
			if(newDepth > myAxis.Max || newDepth < myAxis.Min)
			{
				return false;
			}
			Vector3 temp3 = new Vector3(newWidth, newHeight, newDepth);

			pt1 = temp1;
			pt2 = temp2;
			pt3 = temp3;

			return true;
	}
}
