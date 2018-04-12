using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class GeneratePlanePts : MonoBehaviour {

	public float a, b, c, d;
	public Vector3 pt1, pt2, pt3;
	public Vector3 center;
	public Transform point1, point2, point3;
	public Transform centerPt;
	public PresentPlane presentPlane;
	public PtManager ptManager;
	public float steps = 3;
	public float stepSize = 5;
	public AxisLabelManager xLabelManager;
    public AxisLabelManager yLabelManager;
	public AxisLabelManager zLabelManager;

	public void eqnToPoints() 
	{	
		getCenter();
		changeScale();
		bool check = getSidePoints(null, center.y, center.z);
		Debug.Log("checking x axis " + check);
		if (check == false)
		{
			check = getSidePoints(center.x, null, center.z);
			Debug.Log("checking y " + check);
		}
		if (check == false)
		{
			check = getSidePoints(center.x, center.y, null);
			Debug.Log("checking z " + check);
		}
		ptManager.eqnUpdatePoint(pt1, pt2, pt3);
	}

	public void getCenter() 
	{
		Vector3 normal = new Vector3(a,b,c); 
		float sqrtNormal = normal.sqrMagnitude;
		float x0 = (a*d)/(sqrtNormal);
		float y0 = (b*d)/(sqrtNormal);
		float z0 = (c*d)/(sqrtNormal);
		center = new Vector3(x0, y0, z0);
		presentPlane.center = center;
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
			return checkPtValid((float)y, (float)z, b, a, c, xLabelManager); 
		}
		else if (y == null)
		{
			if (b == 0)
			{
				return false;
			}
			return checkPtValid((float)x, (float)z, a, b, c, yLabelManager);
		}
		else
		{
			if (c == 0)
			{
				return false;
			}
			return checkPtValid((float)x, (float)y, a, c, b, zLabelManager);
		}
	}

	public bool checkPtValid(float width, float height, float widthCoef, float depthCoef, float heightCoef, AxisLabelManager myAxis)
	{
			float newWidth = width-stepSize; 
			float newHeight = height-stepSize;
			float newDepth = (d - widthCoef*newWidth - heightCoef*newHeight)/depthCoef;
			if(newDepth > myAxis.Max || newDepth < myAxis.Min)
			{
				return false;
			}
			Vector3 temp1 = new Vector3(newWidth, newHeight, newDepth);

			newWidth = width+stepSize; 
			newHeight = height-stepSize;
			newDepth = (d - widthCoef*newWidth - heightCoef*newHeight)/depthCoef;
			if(newDepth > myAxis.Max || newDepth < myAxis.Min)
			{
				return false;
			}
			Vector3 temp2 = new Vector3(newWidth, newHeight, newDepth);

			newHeight = height+stepSize;
			newDepth = (d - widthCoef*width - heightCoef*newHeight)/depthCoef;
			if(newDepth > myAxis.Max || newDepth < myAxis.Min)
			{
				return false;
			}
			Vector3 temp3 = new Vector3(width, newHeight, newDepth);

			pt1 = temp1;
			pt2 = temp2;
			pt3 = temp3;

			return true;
	}
}
