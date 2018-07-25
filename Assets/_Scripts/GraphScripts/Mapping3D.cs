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

    const ExpressionSet.ExpOptions X = ExpressionSet.ExpOptions.X;
    const ExpressionSet.ExpOptions Y = ExpressionSet.ExpOptions.Y;
    const ExpressionSet.ExpOptions Z = ExpressionSet.ExpOptions.Z;

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
        Dictionary<string, RangePair> _params = calcManager.expressionSet.ranges;
        float min, max;
        if (ULabelManager != null)
        {

            min = -10 + (_params["u"].Min.Exclusive ? exclusivemodifier : 0);
            max = 10 - (_params["u"].Max.Exclusive ? exclusivemodifier : 0);
            uvwSelector.SetXRange(min, max);
        }
        if (VLabelManager != null)
        {

            min = -10 + (_params["v"].Min.Exclusive ? exclusivemodifier : 0);
            max = 10 - (_params["v"].Max.Exclusive ? exclusivemodifier : 0);
            uvwSelector.SetZRange(min, max);
        }
        if (WLabelManager != null)
        {

            min = -10 + (_params["w"].Min.Exclusive ? exclusivemodifier : 0);
            max = 10 - (_params["w"].Max.Exclusive ? exclusivemodifier : 0);
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
        Dictionary<string, RangePair> _params = calcManager.expressionSet.ranges;
        if (_params.ContainsKey("u"))
            temp.x = (10 + xyz.x) * (_params["u"].Max.Value - _params["u"].Min.Value) / 20 + _params["u"].Min.Value;
        if (_params.ContainsKey("v"))
            temp.y = (10 + xyz.y) * (_params["v"].Max.Value - _params["v"].Min.Value) / 20 + _params["v"].Min.Value;
        if (_params.ContainsKey("w"))
            temp.z = (10 + xyz.z) * (_params["w"].Max.Value - _params["w"].Min.Value) / 20 + _params["w"].Min.Value;
        return temp;
    }

    public Vector3 findXYZ(Vector3 uvw)
    {
        Vector3 output = new Vector3();
        ExpressionSet es = calcManager.expressionSet;
        float scale = calcManager.paramSurface.currentScale;

        foreach (ExpressionSet.ExpOptions key in es.expressions.Keys)
        {
            AK.ExpressionSolver solver = es.solver;
            if (es.ranges.ContainsKey("u")) solver.SetGlobalVariable("u", uvw.x);
            if (es.ranges.ContainsKey("v")) solver.SetGlobalVariable("v", uvw.y);
            if (es.ranges.ContainsKey("w")) solver.SetGlobalVariable("w", uvw.z);
        }
        if (es.IsCompiled())
        {
            output.x = (float)es.expressions[X].AKExpression.Evaluate();
            output.y = (float)es.expressions[Y].AKExpression.Evaluate();
            output.z = (float)es.expressions[Z].AKExpression.Evaluate();
        }
        return output * scale;
    }


    void ManageText()
    {
        Dictionary<string, RangePair> _params = calcManager.expressionSet.ranges;
        if (_params.ContainsKey("u") && ULabelManager != null)
        {
            ULabelManager.Min = _params["u"].Min.Value;
            ULabelManager.Max = _params["u"].Max.Value;
        }
        if (_params.ContainsKey("v") && VLabelManager != null)
        {
            VLabelManager.Max = _params["v"].Max.Value;
            VLabelManager.Min = _params["v"].Min.Value;
        }
        if (_params.ContainsKey("w") && WLabelManager != null)
        {
            WLabelManager.Min = _params["w"].Min.Value;
            WLabelManager.Max = _params["w"].Max.Value;
        }


    }

    public Vector3 swapYandZ(Vector3 toswap)
    {
        return new Vector3(toswap.x, toswap.z, toswap.y);
    }
}
