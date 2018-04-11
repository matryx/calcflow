using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ToggleExpression : QuickButton
{
    Expressions expressions;
    ExpressionBody selectedBody;
    CalculatorManager calcManager;
    ExpressionSet expressionSet;
    Transform selectedExpression;
    Transform expressionActions;
    Transform thisExpr;
    ExpressionComponent expComp;
    ExpressionBody thisBody;
    ParametricExpression param;

    Material showMat, hideMat;
    Color grayHide, grayShow;

    bool active = true;
    
    protected override void Start()
    {
        base.Start();
        expressions = Expressions._instance;
        calcManager = CalculatorManager._instance;
        expressionSet = expressions.getSelectedExprSet();
        expComp = transform.parent.parent.parent.GetComponent<ExpressionComponent>();
        thisExpr = expComp.getExpressionParent();
        thisBody = transform.parent.parent.parent.Find("Button_Input").GetComponent<ExpressionBody>();
        expressionActions = transform.parent.parent.Find("Body");

        showMat = transform.GetComponent<Renderer>().material;
        hideMat = Resources.Load("Icons/HideMat", typeof(Material)) as Material; 
        ColorUtility.TryParseHtmlString("#9E9E9EFF", out grayShow);
        ColorUtility.TryParseHtmlString("#D4D4D4FF", out grayHide);
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        selectedExpression = expressions.getSelectedExpr();
        selectedBody = expressions.getSelectedBody();

        if (active)     //HIDE
        {
            expressionSet = expressions.getSelectedExprSet();
            calcManager.RemoveExpressionSet(expressionSet);
            param = selectedExpression.GetComponent<ParametricExpression>();
            param.setTextColor(grayHide);
            expressionActions.GetComponent<ExpressionActions>().disableButtons();
            param.setButtonInputColor(grayHide);
            param.setActiveStatus(false);
            selectedBody.deselectCurrBody();
        }
        else            //SHOW
        {
            calcManager.AddExpressionSet(expressionSet);
            param.setActiveStatus(true);
            expressions.setSelectedExpr(thisExpr, thisBody);
            thisBody.selectBody();
            selectedExpression = expressions.getSelectedExpr();
            selectedExpression.GetComponent<ParametricExpression>().setTextColor(Color.black);
            selectedExpression.GetComponent<ParametricExpression>().setButtonInputColor(grayShow);
        }

        transform.GetComponent<Renderer>().material = (active) ? hideMat : showMat;
        active = !active;
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
