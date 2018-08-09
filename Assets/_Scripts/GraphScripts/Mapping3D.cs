using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Mapping3D : MonoBehaviour
{

    public ConstraintGrabbable uvwSelector;
    public CalcManager calcManager;
    public Transform CorrespondingPoint;

    public AxisLabelManager ULabelManager;
    public AxisLabelManager VLabelManager;
    public AxisLabelManager WLabelManager;

    public TMPro.TextMeshPro tmpro;

    // Use this for initialization
    void Awake()
    {
        //uvwSelector = GetComponentInChildren<ConstraintGrabbable>();
    }

    // Update is called once per frame
    void Update()
    {
        if (paramSurfaceReady())
        {
            ApplyRangeAdjustements();
            Vector3 xyz = findXYZ(findUVW(swapYandZ(uvwSelector.lastLocalPos)));
            CorrespondingPoint.transform.localPosition = swapYandZ(xyz);
            if (tmpro != null)
            {
                tmpro.text = "(x,y,z) = (" + System.String.Format("{0:F2}", xyz.x) + ","
                                           + System.String.Format("{0:F2}", xyz.y) + ","
                                           + System.String.Format("{0:F2}", xyz.z) + ")";
            }
            ManageText();
        }
    }

    public float exclusivemodifier = .01f;

    void ApplyRangeAdjustements()
    {
        ExpressionSet es = calcManager.expressionSet;

        float min, max;
        if (ULabelManager != null)
        {

            min = -10 + (es.GetRange("u").Min.Exclusive ? exclusivemodifier : 0);
            max = 10 - (es.GetRange("u").Max.Exclusive ? exclusivemodifier : 0);
            uvwSelector.SetXRange(min, max);
        }
        if (VLabelManager != null)
        {

            min = -10 + (es.GetRange("v").Min.Exclusive ? exclusivemodifier : 0);
            max = 10 - (es.GetRange("v").Max.Exclusive ? exclusivemodifier : 0);
            uvwSelector.SetZRange(min, max);
        }
        if (WLabelManager != null)
        {

            min = -10 + (es.GetRange("w").Min.Exclusive ? exclusivemodifier : 0);
            max = 10 - (es.GetRange("w").Max.Exclusive ? exclusivemodifier : 0);
            uvwSelector.SetYRange(min, max);
        }

    }

    public bool paramSurfaceReady()
    {
        return calcManager != null && calcManager.expressionSet != null;
    }

    public Vector3 findUVW(Vector3 xyz)
    {
        Vector3 temp = Vector3.zero;
        ExpressionSet es = calcManager.expressionSet;
        if (es.GetRangeKeys().Contains("u"))
            temp.x = (10 + xyz.x) * (es.GetRange("u").Max.Value - es.GetRange("u").Min.Value) / 20 + es.GetRange("u").Min.Value;
        if (es.GetRangeKeys().Contains("v"))
            temp.y = (10 + xyz.y) * (es.GetRange("v").Max.Value - es.GetRange("v").Min.Value) / 20 + es.GetRange("v").Min.Value;
        if (es.GetRangeKeys().Contains("w"))
            temp.z = (10 + xyz.z) * (es.GetRange("w").Max.Value - es.GetRange("w").Min.Value) / 20 + es.GetRange("w").Min.Value;
        return temp;
    }

    public Vector3 findXYZ(Vector3 uvw)
    {
        Vector3 output = new Vector3();
        ExpressionSet es = calcManager.expressionSet;
        float scale = CartesianManager._instance.GetScale();

        foreach (string key in es.GetExprKeys())
        {
            AK.ExpressionSolver solver = es.solver;
            if (es.GetRangeKeys().Contains("u")) solver.SetGlobalVariable("u", uvw.x);
            if (es.GetRangeKeys().Contains("v")) solver.SetGlobalVariable("v", uvw.y);
            if (es.GetRangeKeys().Contains("w")) solver.SetGlobalVariable("w", uvw.z);
        }
        if (es.IsCompiled())
        {
            output.x = (float)es.GetExpression("X").AKExpression.Evaluate();
            output.y = (float)es.GetExpression("Y").AKExpression.Evaluate();
            output.z = (float)es.GetExpression("Z").AKExpression.Evaluate();
        }
        return output * scale;
    }


    void ManageText()
    {
        ExpressionSet es = calcManager.expressionSet;

        if (es.GetRangeKeys().Contains("u") && ULabelManager != null)
        {
            ULabelManager.Min = es.GetRange("u").Min.Value;
            ULabelManager.Max = es.GetRange("u").Max.Value;
        }
        if (es.GetRangeKeys().Contains("v") && VLabelManager != null)
        {
            VLabelManager.Max = es.GetRange("v").Max.Value;
            VLabelManager.Min = es.GetRange("v").Min.Value;
        }
        if (es.GetRangeKeys().Contains("w") && WLabelManager != null)
        {
            WLabelManager.Min = es.GetRange("w").Min.Value;
            WLabelManager.Max = es.GetRange("w").Max.Value;
        }


    }

    public Vector3 swapYandZ(Vector3 toswap)
    {
        return new Vector3(toswap.x, toswap.z, toswap.y);
    }
}
