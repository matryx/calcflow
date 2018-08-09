using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Mapping1D : MonoBehaviour
{

    public ConstraintGrabbable uvwSelector;
    public CalcManager calcManager;
    public Transform CorrespondingPoint;

    public AxisLabelManager TLabelManager;

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
            CorrespondingPoint.transform.localPosition = swapYandZ(findXYZ(findUVW(swapYandZ(uvwSelector.lastLocalPos))));
            ManageText();
        }
    }

    public float exclusivemodifier = .01f;

    void ApplyRangeAdjustements()
    {
        float min, max;
        ExpressionSet es = calcManager.expressionSet;

        if (TLabelManager != null)
        {

            min = -10 + (es.GetRange("t").Min.Exclusive ? exclusivemodifier : 0);
            max = 10 - (es.GetRange("t").Max.Exclusive ? exclusivemodifier : 0);
            uvwSelector.SetXRange(min, max);
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
        if (es.GetRangeKeys().Contains("t"))
            temp.x = (10 + xyz.x) * (es.GetRange("t").Max.Value - es.GetRange("t").Min.Value) / 20 + es.GetRange("t").Min.Value;
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
            if (es.GetRangeKeys().Contains("t")) solver.SetGlobalVariable("t", uvw.x);
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
        if (es.GetRangeKeys().Contains("t") && TLabelManager != null)
        {
            TLabelManager.Min = es.GetRange("t").Min.Value;
            TLabelManager.Max = es.GetRange("t").Max.Value;
        }
    }

    public Vector3 swapYandZ(Vector3 toswap)
    {
        return new Vector3(toswap.x, toswap.z, toswap.y);
    }
}
