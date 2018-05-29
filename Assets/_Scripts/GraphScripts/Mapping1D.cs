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
        RangePair range_t = calcManager.expressionSet.GetRange("t");
        float min, max;
        if (TLabelManager != null)
        {

            min = -10 + (range_t.Min.Exclusive ? exclusivemodifier : 0);
            max = 10 - (range_t.Max.Exclusive ? exclusivemodifier : 0);
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
        RangePair range_t = calcManager.expressionSet.GetRange("t");

        if (calcManager.expressionSet.GetRange("t") != null)
            temp.x = (10 + xyz.x) * (range_t.Max.Value - range_t.Min.Value) / 20 + range_t.Min.Value;
        return temp;
    }

    public Vector3 findXYZ(Vector3 uvw)
    {
        Vector3 output = new Vector3();
        ExpressionSet es = calcManager.expressionSet;
        float scale = calcManager.paramSurface.currentScale;

        foreach (string key in es.GetExprKeys())
        {
            AK.ExpressionSolver solver = es.solver;
            if (es.GetRange("t") != null) solver.SetGlobalVariable("t", uvw.x);
        }
        if (es.GetExpression("X").AKExpression != null &&
            es.GetExpression("Y").AKExpression != null &&
            es.GetExpression("Z").AKExpression != null)
        {
            output.x = (float)es.GetExpression("X").AKExpression.Evaluate();
            output.y = (float)es.GetExpression("Y").AKExpression.Evaluate();
            output.z = (float)es.GetExpression("Z").AKExpression.Evaluate();
        }
        return output * scale;
    }


    void ManageText()
    {
        RangePair range_t = calcManager.expressionSet.GetRange("t");
        if (calcManager.expressionSet.GetRange("t") != null && TLabelManager != null)
        {
            TLabelManager.Min = range_t.Min.Value;
            TLabelManager.Max = range_t.Max.Value;
        }
    }

    public Vector3 swapYandZ(Vector3 toswap)
    {
        return new Vector3(toswap.x, toswap.z, toswap.y);
    }
}
