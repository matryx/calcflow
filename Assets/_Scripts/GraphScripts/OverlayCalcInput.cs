using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;

public class OverlayCalcInput : MonoBehaviour {

    public CalcManager calcManager;
    public VectorInputManager vecManager;
    public TMP_InputField xInput;
    public TMP_InputField yInput;
    public TMP_InputField zInput;
    public TMP_InputField uInput;
    public TMP_InputField vInput;
    public TMP_InputField wInput;
    public TMP_InputField tInput;

	// Use this for initialization
	void Start () {
		
	}
    
    // Update is called once per frame
    void Update() {
        if (Input.GetKeyDown(KeyCode.Return))
        {
            if (calcManager != null)
            {
                ToExpressionSet(calcManager.expressionSet);
                calcManager.PresetPressed();
            }
            if(vecManager != null)
            {
                ToExpressionSet(vecManager.es);
                vecManager.UpdateEquation();
            }
        }
    }
    private void LateUpdate()
    {
        if (calcManager != null && calcManager.updateOverlay)
        {
            calcManager.updateOverlay = false;
            ExpressionSetToText(calcManager.expressionSet);
        }
        if(vecManager != null && vecManager.overlayUpdate)
        {
            vecManager.overlayUpdate = false;
            ExpressionSetToText(vecManager.es);
        }
    }

    void ExpressionSetToText(ExpressionSet es)
    {
        if (xInput != null)
        {
            xInput.text = es.GetExpression("X").expression;
        }
        if (yInput != null)
        {
            yInput.text = es.GetExpression("Y").expression;
        }
        if (zInput != null)
        {
            zInput.text = es.GetExpression("Z").expression;
        }
        if (uInput != null)
        {
            uInput.text = RangePairToText(es.GetRange("u"));
        }
        if (vInput != null)
        {
            vInput.text = RangePairToText(es.GetRange("v"));
        }
        if (wInput != null)
        {
            wInput.text = RangePairToText(es.GetRange("w"));
        }
        if (tInput != null)
        {
            tInput.text = RangePairToText(es.GetRange("t"));
        }
    }

    void ToExpressionSet(ExpressionSet es)
    {
        List<string> tokens = ExpressionParser.Parse(xInput.text);
        es.AddExpression("X", new Expression(tokens));
        tokens = ExpressionParser.Parse(yInput.text);
        es.AddExpression("Y", new Expression(tokens));
        tokens = ExpressionParser.Parse(zInput.text);
        es.AddExpression("Z", new Expression(tokens));

        if (uInput != null)
        {
            es.AddRange("u", ToRangePair(uInput.text));
        }
        if (vInput != null)
        {
            es.AddRange("v", ToRangePair(vInput.text));
        }
        if (wInput != null)
        {
            es.AddRange("w", ToRangePair(wInput.text));
        }
        if (tInput != null)
        {
            es.AddRange("t", ToRangePair(tInput.text));
        }

        //return es;
    }

    RangePair ToRangePair(string input)
    {
        string[] tokens = input.Split(new char[] { ' ', '[', '(', ',', ')', ']' }, System.StringSplitOptions.RemoveEmptyEntries);
        //foreach(string token in tokens)
        //{
        //    Debug.Log(token);
        //}
        if (tokens.Length != 2) { return null; }
        Range min = new Range(ExpressionParser.Parse(tokens[0]));
        if (input[0] == '[')
        {
            min.Exclusive = false;
        }
        else
        {
            min.Exclusive = true;
        }
        Range max = new Range(ExpressionParser.Parse(tokens[1]));
        if (input[input.Length - 1] == ']')
        {
            max.Exclusive = false;
        }
        else
        {
            max.Exclusive = true;
        }
        return new RangePair(min, max);
    }

    string RangePairToText(RangePair range)
    {
        string str = "";
        if (range.Min.Exclusive)
        {
            str += "(";
        }
        else
        {
            str += "[";
        }
        str += range.Min.expression;
        str += ",";
        str += range.Max.expression;
        if (range.Max.Exclusive)
        {
            str += ")";
        }
        else
        {
            str += "]";
        }
        return str;
    }
}
