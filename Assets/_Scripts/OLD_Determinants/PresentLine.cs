using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Determinants
{
    public class PresentLine : MonoBehaviour
    {

        public Transform point1, point2, point3;
        public Transform point1Line, point2Line, point3Line;
        public Transform centerPt;
        public TextMesh pt1Label, pt2Label, pt3Label;

        public Transform line;
        public Transform forwardLine;
        public Transform lineT;
        public Transform forwardLineT;
        //public Transform backwardLine;
        public Transform lookAtTarget;
        public Transform lookAtTargetT;
        public List<GameObject> walls;
        public MatrixYFX getBasisVectors;

        public PtManager ptManager;
        private PtCoord rawPt1, rawPt2, rawPt3;

        public Vector3 center;
        public Vector3 vector12, vector13, vector23;
        public Vector3 scaledPt1, scaledPt2, scaledPt3;
        public Vector3 scaledPt1Line, scaledPt2Line, scaledPt3Line;
        public Vector3 scaledVector12, scaledVector13;
        public Vector3 normalVector;
        public Vector3 scaledNormal;

        public Vector3 p1, p2, p3;
        public Vector3 p1Line, p2Line, p3Line;

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
            // Debug.Log(MatrixYFX.forLine);
            if(transform.gameObject.name == "PlaneExpression_null")
            {
                // p1 = MatrixYFX.n1;
                // scaledPt1 = ScaledPoint(p1);
                // point1.localPosition = scaledPt1;

                // p2 = MatrixYFX.n2;
                // scaledPt2 = ScaledPoint(p2);
                // point2.localPosition = scaledPt2;

                // p3 = MatrixYFX.n3;
                // scaledPt3 = ScaledPoint(p3);
                // point3.localPosition = scaledPt3;

                // p1Line = MatrixYFX.c1T;
                if(MatrixYFX.mRank==2)
                {
                p1Line = getBasisVectors.n1;
                scaledPt1Line = ScaledPoint(p1Line);
                point1Line.localPosition = scaledPt1Line;

                // p2Line = MatrixYFX.c2T;
                p2Line = getBasisVectors.n2;
                scaledPt2Line = ScaledPoint(p2Line);
                point2Line.localPosition = scaledPt2Line;

                // p3Line = MatrixYFX.c3T;
                p3Line = getBasisVectors.n3;
                scaledPt3Line = ScaledPoint(p3Line);
                point3Line.localPosition = scaledPt3Line;
                }

                if(MatrixYFX.mRank==1)
                {
                p1Line = getBasisVectors.c1T;
                scaledPt1Line = ScaledPoint(p1Line);
                point1Line.localPosition = scaledPt1Line;

                // p2Line = MatrixYFX.c2T;
                p2Line = getBasisVectors.c2T;
                scaledPt2Line = ScaledPoint(p2Line);
                point2Line.localPosition = scaledPt2Line;

                // p3Line = MatrixYFX.c3T;
                p3Line = getBasisVectors.c3T;
                scaledPt3Line = ScaledPoint(p3Line);
                point3Line.localPosition = scaledPt3Line;
                }



                lookAtTarget.localPosition = ScaledPoint(p1Line);
                line.LookAt(lookAtTarget);
                line.localPosition = ScaledPoint(new Vector3(0, 0, 0));
                lineT.LookAt(lookAtTarget);
                lineT.localPosition = ScaledPoint(new Vector3(0, 0, 0));

            }

            if(transform.gameObject.name == "PlaneExpression_col") 
            {
                // p1 = MatrixYFX.c1;
                // scaledPt1 = ScaledPoint(p1);
                // point1.localPosition = scaledPt1;

                // p2 = MatrixYFX.c2;
                // scaledPt2 = ScaledPoint(p2);
                // point2.localPosition = scaledPt2;

                // p3 = MatrixYFX.c3;
                // scaledPt3 = ScaledPoint(p3);
                // point3.localPosition = scaledPt3;

                if(MatrixYFX.mRank==2)
                {
                p1Line = getBasisVectors.n1T;
                // p1Line = MatrixYFX.c1;
                scaledPt1Line = ScaledPoint(p1Line);
                point1Line.localPosition = scaledPt1Line;

                p2Line = getBasisVectors.n2T;
                // p2Line = MatrixYFX.c2;
                scaledPt2Line = ScaledPoint(p2Line);
                point2Line.localPosition = scaledPt2Line;

                p3Line = getBasisVectors.n3T;
                // p3Line = MatrixYFX.c3;
                scaledPt3Line = ScaledPoint(p3Line);
                point3Line.localPosition = scaledPt3Line;
                }

                if(MatrixYFX.mRank==1)
                {
                p1Line = getBasisVectors.c1;
                scaledPt1Line = ScaledPoint(p1Line);
                point1Line.localPosition = scaledPt1Line;

                // p2Line = MatrixYFX.c2T;
                p2Line = getBasisVectors.c2;
                scaledPt2Line = ScaledPoint(p2Line);
                point2Line.localPosition = scaledPt2Line;

                // p3Line = MatrixYFX.c3T;
                p3Line = getBasisVectors.c3;
                scaledPt3Line = ScaledPoint(p3Line);
                point3Line.localPosition = scaledPt3Line;
                }

                lookAtTarget.localPosition = ScaledPoint(p1Line);
                line.LookAt(lookAtTarget);
                line.localPosition = ScaledPoint(new Vector3(0, 0, 0));
                lineT.LookAt(lookAtTarget);
                lineT.localPosition = ScaledPoint(new Vector3(0, 0, 0));
            }
            // p1 = MatrixYFX.c1;
            // p2 = MatrixYFX.c2;
            // p3 = MatrixYFX.c3;
            // scaledPt1 = ScaledPoint(p1);
            // point1.localPosition = scaledPt1;
            // //scaledPt2 = ScaledPoint(p2);
            // //point2.localPosition = scaledPt2;
            // scaledPt3 = ScaledPoint(p3);
            // point3.localPosition = scaledPt3;

            pt1Label.text = "(" + p1.x + "," + p1.y + "," + p1.z + ")";
            pt2Label.text = "(" + p2.x + "," + p2.y + "," + p2.z + ")";
            pt3Label.text = "(" + p3.x + "," + p3.y + "," + p3.z + ")";

            // lookAtTarget.localPosition = ScaledPoint(p1);
            // line.LookAt(lookAtTarget);
            // line.localPosition = ScaledPoint(new Vector3(0, 0, 0));

            // lookAtTargetT.localPosition = ScaledPoint(p1Line);
            // lineT.LookAt(lookAtTargetT);
            // lineT.localPosition = ScaledPoint(new Vector3(0, 0, 0));


            //pt1Label.text = "(" + rawPt1.X.Value + "," + rawPt1.Y.Value + "," + rawPt1.Z.Value + ")";
            //pt2Label.text = "(" + rawPt2.X.Value + "," + rawPt2.Y.Value + "," + rawPt2.Z.Value + ")";
            //pt3Label.text = "(" + rawPt3.X.Value + "," + rawPt3.Y.Value + "," + rawPt3.Z.Value + ")";
            //pt2Label.text = string.Format("({0:F3},{1:F3},{2:F3})", rawPt2.X.Value, rawPt2.Y.Value, rawPt2.Z.Value);



            var sharedMaterial = forwardLine.GetComponent<MeshRenderer>().sharedMaterial;
            sharedMaterial.SetInt("_planeClippingEnabled", 1);

            for (int i = 0; i < 6; i++)
            {
                sharedMaterial.SetVector("_planePos" + i, walls[i].transform.position);
                //plane normal vector is the rotated 'up' vector.
                sharedMaterial.SetVector("_planeNorm" + i, walls[i].transform.rotation * Vector3.up);
            }

            sharedMaterial = forwardLineT.GetComponent<MeshRenderer>().sharedMaterial;
            sharedMaterial.SetInt("_planeClippingEnabled", 1);

            for (int i = 0; i < 6; i++)
            {
                sharedMaterial.SetVector("_planePos" + i, walls[i].transform.position);
                //plane normal vector is the rotated 'up' vector.
                sharedMaterial.SetVector("_planeNorm" + i, walls[i].transform.rotation * Vector3.up);
            }


            ApplyGraphAdjustment();

        }

        public void ApplyGraphAdjustment()
        {

            //vector23 = GenerateVector(rawPt2, rawPt3);

            //center = (PtCoordToVector(rawPt1) + PtCoordToVector(rawPt2) + PtCoordToVector(rawPt3)) / 3;
            //stepSize = Mathf.Max(vector12.magnitude, vector23.magnitude);

            /////////////
            //p1 = new Vector3(-2, 1, 0);
            //p2 = new Vector3(0, 0, 0);
            //p3 = new Vector3(0, 0, 0);
            // vector23 = p2 - p3;
            // vector12 = p1 - p2;

            // center = (p1 + p2 + p3) / 3;
            // stepSize = Mathf.Max(vector12.magnitude, vector23.magnitude);

            // pt1Label.text = "(" + p1.x + "," + p1.y + "," + p1.z + ")";
            // pt2Label.text = "(" + p2.x + "," + p2.y + "," + p2.z + ")";
            // pt3Label.text = "(" + p3.x + "," + p3.y + "," + p3.z + ")";
            //Debug.Log("ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd");
            //Debug.Log("(" + p1.x + "," + p1.y + "," + p1.z + ")");

            center = Vector3.zero;
            stepSize = 5;
            steps = 3;

            //PtCoord centerPt = new PtCoord(new AxisCoord(centerX), new AxisCoord(centerY), new AxisCoord(centerZ));
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
            //expr = solver.SymbolicateExpression(rawEquation);
        }

        public Vector3 ScaledPoint(Vector3 pt)
        {
            Vector3 result = Vector3.zero;
            //print("raw pt1 position: " + pt);
            result.z = (pt.x - xLabelManager.Min) / (xLabelManager.Max - xLabelManager.Min) * 20 - 10;
            result.x = (pt.y - yLabelManager.Min) / (yLabelManager.Max - yLabelManager.Min) * 20 - 10;
            result.y = (pt.z - zLabelManager.Min) / (zLabelManager.Max - zLabelManager.Min) * 20 - 10;
            return result;
        }

        //public void ApplyUnroundCenter(string ptName, Vector3 newLoc)
        //{
        //    if (ptName.Equals("pt1")) center = (newLoc + PtCoordToVector(rawPt2) + PtCoordToVector(rawPt3)) / 3;
        //    else if (ptName.Equals("pt2")) center = (PtCoordToVector(rawPt1) + newLoc + PtCoordToVector(rawPt3)) / 3;
        //    else if (ptName.Equals("pt3")) center = (PtCoordToVector(rawPt1) + PtCoordToVector(rawPt2) + newLoc) / 3;
        //}

        //public Vector3 GenerateVector(PtCoord pt1, PtCoord pt2)
        //{
        //    Vector3 result = Vector3.zero;
        //    result.x = pt2.X.Value - pt1.X.Value;
        //    result.y = pt2.Y.Value - pt1.Y.Value;
        //    result.z = pt2.Z.Value - pt1.Z.Value;
        //    return result;
        //}

        //// Return the raw string of the equation
        //public bool CalculatePlane()
        //{
        //    rawPt1 = ptManager.ptSet.ptCoords["pt1"];
        //    rawPt2 = ptManager.ptSet.ptCoords["pt2"];
        //    rawPt3 = ptManager.ptSet.ptCoords["pt3"];

        //    vector12 = GenerateVector(rawPt1, rawPt2);
        //    vector13 = GenerateVector(rawPt1, rawPt3);
        //    //Debug.Log("Vector 12 is: " + vector12 +". Vector13 is: " + vector13);
        //    normalVector = Vector3.Cross(vector12, vector13);
        //    if (PlaneValid())
        //    {
        //        forwardLine.GetComponent<MeshRenderer>().enabled = true;
        //        backwardLine.GetComponent<MeshRenderer>().enabled = true;

        //        // Basic formula of the equation
        //        d = rawPt1.X.Value * normalVector.x + rawPt1.Y.Value * normalVector.y + rawPt1.Z.Value * normalVector.z;
        //        // string[] formattedValue = roundString(new float[] {normalVector.x, normalVector.y, normalVector.z});
        //        // // Formatting equation
        //        // if (formattedValue[1][0] != '-') formattedValue[1] = '+' + formattedValue[1];
        //        // if (formattedValue[2][0] != '-') formattedValue[2] = '+' + formattedValue[2];
        //        // rawEquation = formattedValue[0] + "x" + formattedValue[1] + "y" + formattedValue[2] + "z=" + d;
        //        ptManager.updateEqn(normalVector.x, normalVector.y, normalVector.z, d);
        //        return true;
        //    }
        //    else
        //    {
        //        forwardLine.GetComponent<MeshRenderer>().enabled = false;
        //        backwardLine.GetComponent<MeshRenderer>().enabled = false;
        //        // rawEquation = "Invalid Plane";
        //        ptManager.updateEqn();
        //        return false;
        //    }

        //    //Debug.Log("Normal vector is: " + normalVector);
        //}

        //public string[] roundString(float[] input)
        //{
        //    string[] result = new string[input.Length];
        //    for (int i = 0; i < input.Length; i++)
        //    {
        //        string a = input[i].ToString();
        //        string b = string.Format("{0:0.000}", input[i]);
        //        print("comparing: " + a + "  " + b);
        //        if (a.Length <= b.Length)
        //        {
        //            result[i] = a;
        //        }
        //        else
        //        {
        //            result[i] = b;
        //        }
        //    }
        //    return result;
        //}

        //public void GetLocalPoint()
        //{
        //    scaledPt1 = ScaledPoint(PtCoordToVector(rawPt1));
        //    point1.localPosition = scaledPt1;
        //    scaledPt2 = ScaledPoint(PtCoordToVector(rawPt2));
        //    point2.localPosition = scaledPt2;
        //    scaledPt3 = ScaledPoint(PtCoordToVector(rawPt3));
        //    point3.localPosition = scaledPt3;
        //    //centerPt.localPosition = ScaledPoint(center);

        //    ///////////////////////////////////////////
        //    Vector3 p1 = new Vector3(1, 0, 0);
        //    scaledPt1 = ScaledPoint(p1);
        //    point1.localPosition = scaledPt1;
        //    //point1.localPosition = p1;

        //    Vector3 p2 = new Vector3(0, 0, 0);
        //    scaledPt2 = ScaledPoint(p2);
        //    point2.localPosition = scaledPt2;
        //    //point2.localPosition = p2;

        //    Vector3 p3 = new Vector3(0, 0, 0);
        //    scaledPt3 = ScaledPoint(p3);
        //    point3.localPosition = scaledPt3;
        //    //point3.localPosition = p3;
        //    ///////////////////////////////////////////
        //}

        //public void GetPlaneDirection()
        //{
        //    //if (PlaneValid())
        //    ////{
        //    //    scaledVector12 = point2.localPosition - point1.localPosition;
        //    //    scaledVector13 = point3.localPosition - point1.localPosition;
        //    //    scaledNormal = Vector3.Cross(scaledVector12, scaledVector13);
        //        /////////////////////
        //        scaledNormal = point2.localPosition - point1.localPosition;
        //        //////////////
        //        float scale = dummySteps * stepSize / scaledNormal.magnitude;
        //        Vector3 dummyPos = scaledNormal * scale;
        //        //Debug.Log("The Normal vector after scale is: " + dummyPos);
        //        lookAtTarget.localPosition = dummyPos + ScaledPoint(center);
        //        //lookAtTarget.localPosition = new Vector3(0, 0, 4.2f);
        //        //Debug.Log("lookAtTarget.localPosition: " + lookAtTarget.localPosition);
        //        centerPt.localPosition = ScaledPoint(center);
        //        line.localPosition = ScaledPoint(center);
        //    //}

        //}


        //public bool PlaneValid()
        //{
        //    // no points are the same
        //    if (PtCoordToVector(rawPt1) == PtCoordToVector(rawPt2) || PtCoordToVector(rawPt1) == PtCoordToVector(rawPt3) || PtCoordToVector(rawPt2) == PtCoordToVector(rawPt3))
        //    {
        //        return false;
        //    }
        //    vector12 = GenerateVector(rawPt1, rawPt2);
        //    vector13 = GenerateVector(rawPt1, rawPt3);
        //    // points are not in same line
        //    float scale;
        //    if (vector13.x != 0)
        //    {
        //        scale = vector12.x / vector13.x;
        //    }
        //    else if (vector13.y != 0)
        //    {
        //        scale = vector12.y / vector13.y;
        //    }
        //    else
        //    {
        //        scale = vector12.z / vector13.z;
        //    }
        //    Vector3 temp = vector13 * scale;
        //    if (vector12.Equals(temp))
        //    {
        //        return false;
        //    }

        //    return true;
        //}

        //public Vector3 PtCoordToVector(PtCoord pt)
        //{
        //    return (new Vector3(pt.X.Value, pt.Y.Value, pt.Z.Value));
        //}



        //public Vector3 UnscaledPoint(Vector3 pt)
        //{
        //    Vector3 result = Vector3.zero;
        //    print("raw pt1 position: " + pt);
        //    result.x = (pt.z + 10) / 20 * (xLabelManager.Max - xLabelManager.Min) + xLabelManager.Min;
        //    result.y = (pt.x + 10) / 20 * (yLabelManager.Max - yLabelManager.Min) + yLabelManager.Min;
        //    result.z = (pt.y + 10) / 20 * (zLabelManager.Max - zLabelManager.Min) + zLabelManager.Min;
        //    return result;
        //}
    }
}
