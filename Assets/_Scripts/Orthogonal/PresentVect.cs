using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PresentVect : MonoBehaviour
{

    public Transform point1;
    public TextMesh pt1Label;

    public List<GameObject> walls;

    public OrthPtManager ptManager;
    private PtCoord rawPt1;

    public Vector3 center;
    public Vector3 vector12, vector13, vector23;
    public Vector3 scaledPt1, scaledPt2, scaledPt3;
    public Vector3 scaledVector12, scaledVector13;

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
        }
    }

    void Update()
    {
        if (!ptSetExist && ptManager != null && ptManager.ptSet != null)
        {
            ptSetExist = true;
            rawPt1 = ptManager.ptSet.ptCoords["pt1"];

        }


        pt1Label.text = "(" + rawPt1.X.Value + "," + rawPt1.Y.Value + "," + rawPt1.Z.Value + ")";


    }

    public Vector3 GenerateVector(PtCoord pt1, PtCoord pt2)
    {
        Vector3 result = Vector3.zero;
        result.x = pt2.X.Value - pt1.X.Value;
        result.y = pt2.Y.Value - pt1.Y.Value;
        result.z = pt2.Z.Value - pt1.Z.Value;
        return result;
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
        //centerPt.localPosition = ScaledPoint(center);
    }


    public Vector3 PtCoordToVector(PtCoord pt)
    {
        return (new Vector3(pt.X.Value, pt.Y.Value, pt.Z.Value));
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
