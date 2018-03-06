using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ToggleExpression : QuickButton
{
    Expressions expressions;
    CalculatorManager calcManager;
    ExpressionSet expressionSet;
    Transform selectedExpression;

    Material showMat, hideMat;
    Color gray;

    bool active = true;
    
    protected override void Start()
    {
        base.Start();
        expressions = Expressions._instance;
        calcManager = CalculatorManager._instance;
        expressionSet = expressions.getSelectedExprSet();
        showMat = transform.GetComponent<Renderer>().material;
        hideMat = Resources.Load("Icons/HideMat", typeof(Material)) as Material;
        ColorUtility.TryParseHtmlString("#9E9E9EFF", out gray);
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        expressionSet = expressions.getSelectedExprSet();
        selectedExpression = expressions.getSelectedExpr();

        if (active)
        {
            calcManager.RemoveExpressionSet(expressionSet);
            selectedExpression.GetComponent<ParametricExpression>().setTextColor(gray);
        }
        else
        {
            calcManager.AddExpressionSet(expressionSet);
            selectedExpression.GetComponent<ParametricExpression>().setTextColor(Color.black);
        }

        transform.GetComponent<Renderer>().material = (active) ? hideMat : showMat;
        active = !active;
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
