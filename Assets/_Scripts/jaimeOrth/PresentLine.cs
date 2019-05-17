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

        //public ConstraintGrabbable constraintGrabbable;
        public Transform xAxis;
        public Transform yAxis;
        public Transform zAxis;


        public Transform line;
        public Transform forwardLine;
        public Transform arrowLine;
        public Transform lookAtTarget;

        public Transform axisline;
        public Transform forwardAxisLine;
        public Transform lookAtAxisTarget;

        public Transform projline;
        public Transform arrow;
        public Transform forwardProjLine;
        public Transform lookAtProjTarg;

        public Transform axisline2;
        public Transform forwardAxisLine2;
        public Transform lookAtAxisTarget2;


        public List<GameObject> walls;


        public PtManager ptManager;
        private PtCoord rawPt1, rawPt2, rawPt3;

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

            point1.GetComponent<MeshRenderer>().enabled = false;

            scaledPt1 = ScaledPoint(PtCoordToVector(rawPt1));
            point1.localPosition = scaledPt1;
            scaledPt2 = ScaledPoint(PtCoordToVector(rawPt2));
            point2.localPosition = scaledPt2;
            scaledPt3 = ScaledPoint(PtCoordToVector(rawPt3));
            point3.localPosition = scaledPt3;
            //center = (PtCoordToVector(rawPt1) + PtCoordToVector(rawPt2) + PtCoordToVector(rawPt3)) / 3;
            //centerPt.localPosition = point1.localPosition;
            centerPt.localPosition = ScaledPoint(new Vector3(0,0,0));
            
            //xAxis.localPosition = ScaledPoint(center);
            //yAxis.localPosition = ScaledPoint(center);
            //zAxis.localPosition = ScaledPoint(center);
            // Debug.Log("dddddddddddddddddd: " + PtCoordToVector(rawPt1));
            Debug.Log("point one is: " + point1.localPosition.x + " " + point1.localPosition.y + " " + point1.localPosition.z);


            //p1 = new Vector3(-2, 1, 0);
            //p2 = new Vector3(0, 0, 0);
            //p3 = new Vector3(0, 0, 0);
            //scaledPt1 = ScaledPoint(p1);
            //point1.localPosition = scaledPt1;
           // scaledPt2 = ScaledPoint(p2);
            //point2.localPosition = scaledPt2;
            //scaledPt3 = ScaledPoint(p3);
            //point3.localPosition = scaledPt3;

            //pt1Label.text = "(" + p1.x + "," + p1.y + "," + p1.z + ")";
            // pt2Label.text = "(" + p2.x + "," + p2.y + "," + p2.z + ")";
            //pt3Label.text = "(" + p3.x + "," + p3.y + "," + p3.z + ")";

            //get the points from the users vector and for the line to look at
            lookAtTarget.localPosition = scaledPt1;
            //have the line look at the point
            line.LookAt(lookAtTarget);
            //set the line to have an origin of zero
            line.localPosition = ScaledPoint(new Vector3(0, 0, 0));
            //scale the vector
            line.localScale = new Vector3(1, 1, scaledPt1.magnitude);
            
            //axis 1
            lookAtAxisTarget.localPosition = scaledPt2;
            axisline.LookAt(lookAtAxisTarget);
            axisline.localPosition = ScaledPoint(new Vector3(0, 0, 0));
            //pt2Label.text = "(" + point2.localPosition.x + "," + point2.localPosition.y + "," + point2.localPosition.z + ")";
            Debug.Log("point two is: " + point2.localPosition.x + " " + point2.localPosition.y + " " + point2.localPosition.z);

            //calculate projection axis component
            //vector to project and normal
            projectedResult = Vector3.Project(PtCoordToVector(rawPt1), PtCoordToVector(rawPt2)); // give this back to them?
            //scaled to project
            Vector3 scaledRes = ScaledPoint(projectedResult);

            //Debug.Log("INPUT VECT PTS: " + "(" + rawPt1.X.Value + "," + rawPt1.Y.Value + "," + rawPt1.Z.Value + ")");
            //Debug.Log("NORMAL VECT PTS: " + "(" + rawPt2.X.Value + "," + rawPt2.Y.Value + "," + rawPt2.Z.Value + ")");
            //Debug.Log("PROJECTED VECT PTS: " + "(" + projectedResult.x + "," + projectedResult.y + "," + projectedResult.z + ")");

           
            //show them the projected value
            scaledPt2 = scaledRes; 
            point2.localPosition = scaledPt2; 
            pt2Label.text = "(" + projectedResult.x + "," + projectedResult.y + "," + projectedResult.z + ")";

            point3.GetComponent<MeshRenderer>().enabled = false;

            lookAtProjTarg.localPosition = scaledRes;
            projline.LookAt(lookAtProjTarg);

            projline.localPosition = centerPt.localPosition;

            //projline.localScale = new Vector3(1, 1, scaledRes.x);
            projline.localScale = new Vector3(1, 1, scaledRes.magnitude);

            var sharedMaterialP = forwardProjLine.GetComponent<MeshRenderer>().sharedMaterial;
            sharedMaterialP.SetInt("_planeClippingEnabled", 1);

            for (int i = 0; i < 6; i++)
            {
                sharedMaterialP.SetVector("_planePos" + i, walls[i].transform.position);
                //plane normal vector is the rotated 'up' vector.
                sharedMaterialP.SetVector("_planeNorm" + i, walls[i].transform.rotation * Vector3.up);
            }

            //axis 2 //TODO not working yet (maybe omit and just change the plane anyway?  
            //lookAtAxisTarget2.localPosition = scaledPt3;
            //axisline2.LookAt(lookAtAxisTarget2);
            //axisline2.localPosition = ScaledPoint(new Vector3(0, 0, 0));

            pt1Label.text = "(" + rawPt1.X.Value + "," + rawPt1.Y.Value + "," + rawPt1.Z.Value + ")";
            pt3Label.text = "(" + rawPt3.X.Value + "," + rawPt3.Y.Value + "," + rawPt3.Z.Value + ")";            

            var sharedMaterial = forwardLine.GetComponent<MeshRenderer>().sharedMaterial;
            sharedMaterial.SetInt("_planeClippingEnabled", 1);

            for (int i = 0; i < 6; i++)
            {
                sharedMaterial.SetVector("_planePos" + i, walls[i].transform.position);
                //plane normal vector is the rotated 'up' vector.
                sharedMaterial.SetVector("_planeNorm" + i, walls[i].transform.rotation * Vector3.up);
            }


            var sharedMaterial2 = forwardAxisLine.GetComponent<MeshRenderer>().sharedMaterial;
            sharedMaterial2.SetInt("_planeClippingEnabled", 1);

            for (int i = 0; i < 6; i++)
            {
                sharedMaterial2.SetVector("_planePos" + i, walls[i].transform.position);
                //plane normal vector is the rotated 'up' vector.
                sharedMaterial2.SetVector("_planeNorm" + i, walls[i].transform.rotation * Vector3.up);
            }

      
            //if(transform.gameObject.name == "PlaneExpression_null")
            //{
            //    var sharedMaterial = forwardPlane.GetComponent<MeshRenderer>().sharedMaterial;
            //    sharedMaterial.SetInt("_planeClippingEnabled", 1);

            //    for (int i = 0; i < 6; i++)
            //    {
            //        sharedMaterial.SetVector("_planePos" + i, walls[i].transform.position);
            //        //plane normal vector is the rotated 'up' vector.
            //        sharedMaterial.SetVector("_planeNorm" + i, walls[i].transform.rotation * Vector3.up);
            //    }

            //    sharedMaterial = backwardPlane.GetComponent<MeshRenderer>().sharedMaterial;
            //    sharedMaterial.SetInt("_planeClippingEnabled", 1);

            //    for (int i = 0; i < 6; i++)
            //    {
            //        sharedMaterial.SetVector("_planePos" + i, walls[i].transform.position);
            //        //plane normal vector is the rotated 'up' vector.
            //        sharedMaterial.SetVector("_planeNorm" + i, walls[i].transform.rotation * Vector3.up);
            //    }
            //}



        }

        //public void applygraphadjustment()
        //{

        //    //vector23 = generatevector(rawpt2, rawpt3);

        //    //center = (ptcoordtovector(rawpt1) + ptcoordtovector(rawpt2) + ptcoordtovector(rawpt3)) / 3;
        //    //stepsize = mathf.max(vector12.magnitude, vector23.magnitude);

        //    /////////////
        //    p1 = new vector3(-2, 1, 0);
        //    p2 = new vector3(0, 0, 0);
        //    p3 = new vector3(0, 0, 0);
        //    vector23 = p2 - p3;
        //    vector12 = p1 - p2;

        //    center = (p1 + p2 + p3) / 3;
        //    stepsize = mathf.max(vector12.magnitude, vector23.magnitude);

        //    pt1label.text = "(" + p1.x + "," + p1.y + "," + p1.z + ")";
        //    pt2label.text = "(" + p2.x + "," + p2.y + "," + p2.z + ")";
        //    pt3label.text = "(" + p3.x + "," + p3.y + "," + p3.z + ")";
        //    debug.log("ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd");
        //    debug.log("(" + p1.x + "," + p1.y + "," + p1.z + ")");


        //    //ptcoord centerpt = new ptcoord(new axiscoord(centerx), new axiscoord(centery), new axiscoord(centerz));
        //    //get the range of the box
        //    if (stepsize == 0)
        //    {
        //        stepsize = defaultstepsize;
        //    }
        //    xlabelmanager.min = center.x - stepsize * steps;
        //    ylabelmanager.min = center.y - stepsize * steps;
        //    zlabelmanager.min = center.z - stepsize * steps;
        //    xlabelmanager.max = center.x + stepsize * steps;
        //    ylabelmanager.max = center.y + stepsize * steps;
        //    zlabelmanager.max = center.z + stepsize * steps;
        //    //get the interaction points between the box edges and the plane
        //    //expr = solver.symbolicateexpression(rawequation);
        //}

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

        public void GetLocalPoint()
        {
            scaledPt1 = ScaledPoint(PtCoordToVector(rawPt1));
            point1.localPosition = scaledPt1;
            scaledPt2 = ScaledPoint(PtCoordToVector(rawPt2));
            point2.localPosition = scaledPt2;
            scaledPt3 = ScaledPoint(PtCoordToVector(rawPt3));
            point3.localPosition = scaledPt3;
            
            //centerPt.localPosition = ScaledPoint(center);

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
        }

        //public void GetPlaneDirection()
        //{
        //    //if (PlaneValid())
        //    ////{
        //    //    scaledVector12 = point2.localPosition - point1.localPosition;
        //    //    scaledVector13 = point3.localPosition - point1.localPosition;
        //    //    scaledNormal = Vector3.Cross(scaledVector12, scaledVector13);
        //    /////////////////////
        //    scaledNormal = point2.localPosition - point1.localPosition;
        //    //////////////
        //    float scale = dummySteps * stepSize / scaledNormal.magnitude;
        //    Vector3 dummyPos = scaledNormal * scale;
        //    //Debug.Log("The Normal vector after scale is: " + dummyPos);
        //    lookAtTarget.localPosition = dummyPos + ScaledPoint(center);
        //    //lookAtTarget.localPosition = new Vector3(0, 0, 4.2f);
        //    //Debug.Log("lookAtTarget.localPosition: " + lookAtTarget.localPosition);
        //    centerPt.localPosition = ScaledPoint(center);
        //    line.localPosition = ScaledPoint(center);
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

        public Vector3 PtCoordToVector(PtCoord pt)
        {
            return (new Vector3(pt.X.Value, pt.Y.Value, pt.Z.Value));
        }

        //maintains ratio of each point in the scaled space so the points don't go out of bounds
        public Vector3 ScaledPoint(Vector3 pt)
        {
            Vector3 result = Vector3.zero;
            //print("raw pt1 position: " + pt);
            result.z = (pt.x - xLabelManager.Min) / (xLabelManager.Max - xLabelManager.Min) * 20 - 10;
            result.x = (pt.y - yLabelManager.Min) / (yLabelManager.Max - yLabelManager.Min) * 20 - 10;
            result.y = (pt.z - zLabelManager.Min) / (zLabelManager.Max - zLabelManager.Min) * 20 - 10;
            return result;
        }

        public Vector3 UnscaledPoint(Vector3 pt)
        {
            Vector3 result = Vector3.zero;
            print("raw pt1 position: " + pt);
            result.x = (pt.z + 10) / 20 * (xLabelManager.Max - xLabelManager.Min) + xLabelManager.Min;
            result.y = (pt.x + 10) / 20 * (yLabelManager.Max - yLabelManager.Min) + yLabelManager.Min;
            result.z = (pt.y + 10) / 20 * (zLabelManager.Max - zLabelManager.Min) + zLabelManager.Min;
            return result;
        }
    }
}
