using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace orthProj
{
    public class PresentLine : MonoBehaviour
    {

        public Transform point1, point2, point3;
        public Transform centerPt;
        public TextMesh pt1Label, pt2Label, pt3Label;

        public Transform plane;
        public Transform forwardPlane;
        public Transform backwardPlane;
        public Transform lookAtPlaneTarget;

        public Transform xAxis;
        public Transform xAxisText;
        public Transform yAxis;
        public Transform zAxis;

        public Transform axisline;
        public Transform forwardAxisLine;
        public Transform lookAtAxisTarget;
        public List<GameObject> walls;


        public PtManager ptManager;
        private PtCoord rawPt1, rawPt2, rawPt3, zero;

        public Vector3 usersPoint;
        public Vector3 center;
        public Vector3 projectedResult;
        public Vector3 projectedScaled;
        public Vector3 vector12, vector13, vector23;
        public Vector3 scaledPt1, scaledPt2, scaledPt3;
        public Vector3 scaledVector12, scaledVector13;
        public Vector3 normalVector;
        public Vector3 scaledNormal;

        public Vector3 p1, p2, p3;

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

        bool ptSetExist = false;

        public float d;

        void Awake()
        {
            solver = new AK.ExpressionSolver();
            expr = new AK.Expression();
            vertices = new List<Vector3>();
            stepSize = defaultStepSize;
            if (ptManager != null && ptManager.ptSet != null)
            {
                ptSetExist = true;
                rawPt1 = ptManager.ptSet.ptCoords["pt1"];
                rawPt2 = ptManager.ptSet.ptCoords["pt2"];
                rawPt3 = ptManager.ptSet.ptCoords["pt3"];
                usersPoint = PtCoordToVector(rawPt1);
            }
        }

        void Update()
        {
            if (!ptSetExist && ptManager != null && ptManager.ptSet != null)
            {
                ptSetExist = true;
                rawPt1 = ptManager.ptSet.ptCoords["pt1"];
                rawPt2 = ptManager.ptSet.ptCoords["pt2"];
                rawPt3 = ptManager.ptSet.ptCoords["pt3"];
            }
            usersPoint = PtCoordToVector(rawPt1);

            plane.LookAt(lookAtPlaneTarget);

            //TURN OFF AND ON POINT VISABILITY
            point1.GetComponent<MeshRenderer>().enabled = false;
            point2.GetComponent<MeshRenderer>().enabled = false;
            point3.GetComponent<MeshRenderer>().enabled = false;

            //HERES YOUR COORD TEXT
            pt1Label.text = "(" + rawPt1.X.Value + "," + rawPt1.Y.Value + "," + rawPt1.Z.Value + ")";
            //pt2Label.text = "(" + rawPt2.X.Value + "," + rawPt2.Y.Value + "," + rawPt2.Z.Value + ")";
            //pt3Label.text = "(" + rawPt3.X.Value + "," + rawPt3.Y.Value + "," + rawPt3.Z.Value + ")";

            scaledPt1 = ScaledPoint(PtCoordToVector(rawPt1));
            point1.localPosition = scaledPt1;
            scaledPt2 = ScaledPoint(PtCoordToVector(rawPt2));
            point2.localPosition = scaledPt2;
            scaledPt3 = ScaledPoint(PtCoordToVector(rawPt3));
            point3.localPosition = scaledPt3;

            xAxis.localPosition = ScaledPoint(center);
            yAxis.localPosition = ScaledPoint(center);
            zAxis.localPosition = ScaledPoint(center);

            //projectionAxis
            lookAtAxisTarget.localPosition = ScaledPoint(PtCoordToVector(rawPt2));
            axisline.localPosition = ScaledPoint(center);
            axisline.LookAt(lookAtAxisTarget);

          

            var sharedMaterial2 = forwardAxisLine.GetComponent<MeshRenderer>().sharedMaterial;
            sharedMaterial2.SetInt("_planeClippingEnabled", 1);

            for (int i = 0; i < 6; i++)
            {
                sharedMaterial2.SetVector("_planePos" + i, walls[i].transform.position);
                //plane normal vector is the rotated 'up' vector.
                sharedMaterial2.SetVector("_planeNorm" + i, walls[i].transform.rotation * Vector3.up);
            }

            if((new Vector3(0, 0, 0)) == PtCoordToVector(rawPt2))
            {
                forwardAxisLine.GetComponent<MeshRenderer>().enabled = false;
            }
            //if there is no 3rd component (line)
            else if (new Vector3(0, 0, 0) == PtCoordToVector(rawPt3))
            {
                forwardAxisLine.GetComponent<MeshRenderer>().enabled = true;
                projectedResult = Vector3.Project(PtCoordToVector(rawPt1), PtCoordToVector(rawPt2));
                ptManager.manageText();
            }
            //if there is a 3rd coord (subspace)
            else
            {
                forwardAxisLine.GetComponent<MeshRenderer>().enabled = false;
                projectedResult = Vector3.ProjectOnPlane(PtCoordToVector(rawPt1), normalVector);
                ptManager.manageText();
            }
            
            var sharedMaterial3 = forwardPlane.GetComponent<MeshRenderer>().sharedMaterial;
            sharedMaterial3.SetInt("_planeClippingEnabled", 1);

            for (int i = 0; i < 6; i++)
            {
                sharedMaterial3.SetVector("_planePos" + i, walls[i].transform.position);
                //plane normal vector is the rotated 'up' vector.
                sharedMaterial3.SetVector("_planeNorm" + i, walls[i].transform.rotation * Vector3.up);
            }

            var sharedMaterial = backwardPlane.GetComponent<MeshRenderer>().sharedMaterial;
            sharedMaterial.SetInt("_planeClippingEnabled", 1);

            for (int i = 0; i < 6; i++)
            {
                sharedMaterial.SetVector("_planePos" + i, walls[i].transform.position);
                //plane normal vector is the rotated 'up' vector.
                sharedMaterial.SetVector("_planeNorm" + i, walls[i].transform.rotation * Vector3.up);
            }


        }


        public void ApplyGraphAdjustment()
        {
            center = new Vector3(0, 0, 0);
            stepSize = (center - PtCoordToVector(rawPt1)).magnitude;
            //Get the range of the box
            if (stepSize == 0)
            {
                stepSize = defaultStepSize;
            }
            xLabelManager.Min = center.x - stepSize * steps;
            yLabelManager.Min = center.y - stepSize * steps;
            zLabelManager.Min = center.z - stepSize * steps;
            xLabelManager.Max = center.x + stepSize * steps;
            yLabelManager.Max = center.y + stepSize * steps;
            zLabelManager.Max = center.z + stepSize * steps;
            //Get the interaction points between the box edges and the plane
        }

        public void ApplyUnroundCenter(string ptName, Vector3 newLoc)
        {
            if (ptName.Equals("pt1")) center = (newLoc + PtCoordToVector(rawPt2) + PtCoordToVector(rawPt3)) / 3;
            else if (ptName.Equals("pt2")) center = (PtCoordToVector(rawPt1) + newLoc + PtCoordToVector(rawPt3)) / 3;
            else if (ptName.Equals("pt3")) center = (PtCoordToVector(rawPt1) + PtCoordToVector(rawPt2) + newLoc) / 3;
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
        public bool CalculatePlane()
        {
            rawPt1 = ptManager.ptSet.ptCoords["pt1"];
            rawPt2 = ptManager.ptSet.ptCoords["pt2"];
            rawPt3 = ptManager.ptSet.ptCoords["pt3"];
            
            vector12 = new Vector3(0, 0, 0) - new Vector3(rawPt2.Y.Value, rawPt2.X.Value, rawPt2.Z.Value);  
            vector13 = new Vector3(0, 0, 0) - new Vector3(rawPt3.Y.Value, rawPt3.X.Value, rawPt3.Z.Value);
            //Debug.Log("Vector 12 is: " + vector12 +". Vector13 is: " + vector13);
            normalVector = Vector3.Cross(vector12, vector13);
            if (PlaneValid())
            {
              
                forwardPlane.GetComponent<MeshRenderer>().enabled = true;
                backwardPlane.GetComponent<MeshRenderer>().enabled = true;

                // swapped x y
                // Basic formula of the equation
                d = rawPt1.Y.Value * normalVector.y + rawPt1.X.Value * normalVector.x + rawPt1.Z.Value * normalVector.z;
                ptManager.updateEqn(normalVector.x, normalVector.y, normalVector.z, d);
                return true;
            }
            else
            {
                forwardPlane.GetComponent<MeshRenderer>().enabled = false;
                backwardPlane.GetComponent<MeshRenderer>().enabled = false;
                ptManager.updateEqn();
                return false;
            }


        }

        public string[] roundString(float[] input)
        {
            string[] result = new string[input.Length];
            for (int i = 0; i < input.Length; i++)
            {
                string a = input[i].ToString();
                string b = string.Format("{0:0.000}", input[i]);
                print("comparing: " + a + "  " + b);
                if (a.Length <= b.Length)
                {
                    result[i] = a;
                }
                else
                {
                    result[i] = b;
                }
            }
            return result;
        }

        public void GetLocalPoint()
        {
            scaledPt1 = ScaledPoint(PtCoordToVector(rawPt1));
            point1.localPosition = scaledPt1;
            scaledPt2 = ScaledPoint(PtCoordToVector(rawPt2));
            point2.localPosition = scaledPt2;
            scaledPt3 = ScaledPoint(PtCoordToVector(rawPt3));
            point3.localPosition = scaledPt3;
            
        }

        public void GetPlaneDirection()
        {
            if (PlaneValid())
            {

                scaledVector12 = scaledPt2 - ScaledPoint(center);
                scaledVector13 = scaledPt3 - ScaledPoint(center);
                scaledNormal = Vector3.Cross(scaledVector12, scaledVector13);
                float scale = dummySteps * stepSize / scaledNormal.magnitude;
                Vector3 dummyPos = scaledNormal * scale;
                lookAtPlaneTarget.localPosition = dummyPos + ScaledPoint(center);
                centerPt.localPosition = ScaledPoint(center);
                plane.localPosition = ScaledPoint(center);
            }
        }


        public bool PlaneValid()
        {
            // no points are the same
            if (new Vector3(0, 0, 0) == PtCoordToVector(rawPt2) || new Vector3(0, 0, 0) == PtCoordToVector(rawPt3) || PtCoordToVector(rawPt2) == PtCoordToVector(rawPt3))
            {
                return false;
            }
            vector12 = new Vector3(0, 0, 0) - new Vector3(rawPt2.X.Value, rawPt2.Y.Value, rawPt2.Z.Value);
            vector13 = new Vector3(0, 0, 0) - new Vector3(rawPt3.X.Value, rawPt3.Y.Value, rawPt3.Z.Value);
            // points are not in same line
            float scale;
            if (vector13.y != 0)
            {
                scale = vector12.y / vector13.y;
            }
            else if (vector13.x != 0)
            {
                scale = vector12.x / vector13.x;
            }
            else
            {
                scale = vector12.z / vector13.z;
            }
            Vector3 temp = vector13 * scale;
            if (vector12.Equals(temp))
            {
                return false;
            }

            return true;
        }

        public Vector3 PtCoordToVector(PtCoord pt)
        {
            return (new Vector3(pt.Y.Value, pt.X.Value, pt.Z.Value));
        }

        //maintains ratio of each point in the scaled space so the points don't go out of bounds
        public Vector3 ScaledPoint(Vector3 pt)
        {
            Vector3 result = Vector3.zero;
            //print("raw pt1 position: " + pt);
            result.z = (pt.x - xLabelManager.Min) / (xLabelManager.Max - xLabelManager.Min) * 40 - 20;
            result.x = (pt.y - yLabelManager.Min) / (yLabelManager.Max - yLabelManager.Min) * 40 - 20;
            result.y = (pt.z - zLabelManager.Min) / (zLabelManager.Max - zLabelManager.Min) * 40 - 20;
            return result;
        }

        public Vector3 UnscaledPoint(Vector3 pt)
        {
            Vector3 result = Vector3.zero;
            print("raw pt1 position: " + pt);
            result.x = (pt.z + 20) / 40 * (xLabelManager.Max - xLabelManager.Min) + xLabelManager.Min;
            result.y = (pt.x + 20) / 40 * (yLabelManager.Max - yLabelManager.Min) + yLabelManager.Min;
            result.z = (pt.y + 20) / 40 * (zLabelManager.Max - zLabelManager.Min) + zLabelManager.Min;
            return result;
        }
    }
}
