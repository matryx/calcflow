using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace LinearAlgebraMatrix
{
    public class GeneratePlanePts : MonoBehaviour
    {

        public float a, b, c, d;

        EqnSet eqnSet;
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
        public float sqrtNormal;

        bool ptSetExist = false;

        void Awake()
        {
            if (ptManager != null && ptManager.eqnSet != null)
            {
                ptSetExist = true;
                eqnSet = ptManager.eqnSet;
            }
        }

        void Update()
        {
            if (!ptSetExist && ptManager != null && ptManager.eqnSet != null)
            {
                ptSetExist = true;
                eqnSet = ptManager.eqnSet;
            }
        }

        //public void eqnToPoints()
        //{
        //    getCenter();
        //    changeScale();
        //    bool check = getSidePoints(null, center.y, center.z);
        //    Debug.Log("fixing x axis " + check);
        //    if (check == false)
        //    {
        //        check = getSidePoints(center.x, null, center.z);
        //        Debug.Log("fixing y " + check);
        //    }
        //    if (check == false)
        //    {
        //        check = getSidePoints(center.x, center.y, null);
        //        Debug.Log("fixing z " + check);
        //    }
        //    ptManager.eqnUpdatePoint(pt1, pt2, pt3);
        //}

        public void getCenter()
        {
            a = eqnSet.eqnCoefs["a"].Value;
            b = eqnSet.eqnCoefs["b"].Value;
            c = eqnSet.eqnCoefs["c"].Value;
            d = eqnSet.eqnCoefs["d"].Value;
            Vector3 normal = new Vector3(a, b, c);
            sqrtNormal = normal.sqrMagnitude;
            float x0 = (a * d) / (sqrtNormal);
            float y0 = (b * d) / (sqrtNormal);
            float z0 = (c * d) / (sqrtNormal);
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
            float newX;
            float newY;
            float newZ;
            if (x == null)
            {
                if (a == 0)
                {
                    return false;
                }
                //Lower left point
                newY = (float)y - stepSize;
                newZ = (float)z - stepSize;
                newX = (d - newY * b - newZ * c) / a;
                if (newX > xLabelManager.Max || newX < xLabelManager.Min)
                {
                    return false;
                }
                Vector3 temp1 = new Vector3(newX, newY, newZ);

                //Lower right point
                newY = (float)y + stepSize;
                newZ = (float)z - stepSize;
                newX = (d - newY * b - newZ * c) / a;
                if (newX > xLabelManager.Max || newX < xLabelManager.Min)
                {
                    return false;
                }
                Vector3 temp2 = new Vector3(newX, newY, newZ);

                //Upper Point
                newZ = (float)z + stepSize;
                newX = (d - (float)y * b - newZ * c) / a;
                if (newX > xLabelManager.Max || newX < xLabelManager.Min)
                {
                    return false;
                }
                Vector3 temp3 = new Vector3(newX, (float)y, newZ);

                pt1 = temp1;
                pt2 = temp2;
                pt3 = temp3;

                return true;

            }
            else if (y == null)
            {
                if (b == 0)
                {
                    return false;
                }

                //Lower left point
                newX = (float)x - stepSize;
                newZ = (float)z - stepSize;
                newY = (d - newX * a - newZ * c) / b;
                if (newY > yLabelManager.Max || newY < yLabelManager.Min)
                {
                    return false;
                }
                Vector3 temp1 = new Vector3(newX, newY, newZ);

                //Lower right point
                newX = (float)x + stepSize;
                newZ = (float)z - stepSize;
                newY = (d - newX * a - newZ * c) / b;
                if (newY > yLabelManager.Max || newY < yLabelManager.Min)
                {
                    return false;
                }
                Vector3 temp2 = new Vector3(newX, newY, newZ);

                //Upper Point
                newZ = (float)z + stepSize;
                newY = (d - (float)x * a - newZ * c) / b;
                if (newY > yLabelManager.Max || newY < yLabelManager.Min)
                {
                    return false;
                }
                Vector3 temp3 = new Vector3((float)x, newY, newZ);

                pt1 = temp1;
                pt2 = temp2;
                pt3 = temp3;

                return true;


            }
            else
            {
                if (c == 0)
                {
                    return false;
                }

                //Lower left point
                newX = (float)x - stepSize;
                newY = (float)y - stepSize;
                newZ = (d - newX * a - newY * b) / c;
                if (newZ > zLabelManager.Max || newZ < zLabelManager.Min)
                {
                    return false;
                }
                Vector3 temp1 = new Vector3(newX, newY, newZ);

                //Lower right point
                newX = (float)x + stepSize;
                newY = (float)y - stepSize;
                newZ = (d - newX * a - newY * b) / c;
                if (newZ > zLabelManager.Max || newZ < zLabelManager.Min)
                {
                    return false;
                }
                Vector3 temp2 = new Vector3(newX, newY, newZ);

                //Upper Point
                newY = (float)y + stepSize;
                newZ = (d - (float)x * a - newY * b) / c;
                if (newZ > zLabelManager.Max || newZ < zLabelManager.Min)
                {
                    return false;
                }
                Vector3 temp3 = new Vector3((float)x, newY, newZ);

                pt1 = temp1;
                pt2 = temp2;
                pt3 = temp3;

                return true;

            }
        }

    }
}
