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
        Dictionary<string, RangePair> _params = calcManager.expressionSet.ranges;
        float min, max;
        if (TLabelManager != null)
        {

            min = -10 + (_params["t"].Min.Exclusive ? exclusivemodifier : 0);
            max = 10 - (_params["t"].Max.Exclusive ? exclusivemodifier : 0);
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
        Dictionary<string, RangePair> _params = calcManager.expressionSet.ranges;
        if (_params.ContainsKey("t"))
            temp.x = (10 + xyz.x) * (_params["t"].Max.Value - _params["t"].Min.Value) / 20 + _params["t"].Min.Value;
        return temp;
    }

    public Vector3 findXYZ(Vector3 uvw)
    {
        Vector3 output = new Vector3();
        ExpressionSet es = calcManager.expressionSet;
        float scale = calcManager.paramSurface.currentScale;

        foreach (string key in es.expressions.Keys)
        {
            AK.ExpressionSolver solver = es.solver;
            if (es.ranges.ContainsKey("t")) solver.SetGlobalVariable("t", uvw.x);
        }
        if (es.IsCompiled())
        {
            output.x = (float)es.expressions["X"].AKExpression.Evaluate();
            output.y = (float)es.expressions["Y"].AKExpression.Evaluate();
            output.z = (float)es.expressions["Z"].AKExpression.Evaluate();
        }
        return output * scale;
    }


    void ManageText()
    {
        Dictionary<string, RangePair> _params = calcManager.expressionSet.ranges;
        if (_params.ContainsKey("t") && TLabelManager != null)
        {
            TLabelManager.Min = _params["t"].Min.Value;
            TLabelManager.Max = _params["t"].Max.Value;
        }
    }

    public Vector3 swapYandZ(Vector3 toswap)
    {
        return new Vector3(toswap.x, toswap.z, toswap.y);
    }
}
