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
    
    Material showMat, hideMat;
    Color grayHide, grayShow;

    bool active = true;
    
    protected override void Start()
    {
        base.Start();
        expressions = Expressions._instance;
        calcManager = CalculatorManager._instance;
        expressionSet = expressions.getSelectedExprSet();

        expressionActions = transform.parent.parent.Find("Body");

        showMat = transform.GetComponent<Renderer>().material;
        hideMat = Resources.Load("Icons/HideMat", typeof(Material)) as Material; 
        ColorUtility.TryParseHtmlString("#9E9E9EFF", out grayShow);
        ColorUtility.TryParseHtmlString("#D4D4D4FF", out grayHide);
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        expressionSet = expressions.getSelectedExprSet();
        selectedExpression = expressions.getSelectedExpr();
        selectedBody = expressions.getSelectedBody();

        if (active)     //HIDE
        {
            print("HIDING");
            calcManager.RemoveExpressionSet(expressionSet);
            selectedExpression.GetComponent<ParametricExpression>().setTextColor(grayHide);
            //expressionActions.GetComponent<Renderer>().material.SetTexture("_MainTex", Resources.Load("Icons/finalEllipses_gray", typeof(Texture)) as Texture);
            expressionActions.GetComponent<ExpressionActions>().disableButtons();
            selectedExpression.GetComponent<ParametricExpression>().setButtonInputColor(grayHide);
            selectedBody.deselectCurrBody();  
        }
        else            //SHOW
        {
            print("SHOWING");
            calcManager.AddExpressionSet(expressionSet);
            //BUG: not the selected expression so this is null
            selectedExpression.GetComponent<ParametricExpression>().setTextColor(Color.black);
            //expressionActions.GetComponent<Renderer>().material.SetTexture("_MainTex", Resources.Load("Icons/finalEllipses", typeof(Texture)) as Texture);
            selectedExpression.GetComponent<ParametricExpression>().setButtonInputColor(grayShow);
            //TODO: select expression, component X
        }

        transform.GetComponent<Renderer>().material = (active) ? hideMat : showMat;
        active = !active;
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
