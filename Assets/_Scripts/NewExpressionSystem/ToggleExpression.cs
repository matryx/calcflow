using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ToggleExpression : QuickButton
{
    Expressions expressions;
    CalculatorManager calcManager;
    ExpressionSet expressionSet;
    Transform expressionActions;
    Transform thisExpr;
    ExpressionComponent expComp;
    ExpressionBody thisBody;
    ParametricExpression param;

    Material showMat, hideMat;
    Texture quadShow, quadHide;
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
        quadShow = Resources.Load("Icons/element", typeof(Texture2D)) as Texture;
        quadHide = Resources.Load("Icons/gray_element", typeof(Texture2D)) as Texture;
        ColorUtility.TryParseHtmlString("#9E9E9EFF", out grayShow);
        ColorUtility.TryParseHtmlString("#D4D4D4FF", out grayHide);
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (active)     //HIDE
        {
            if (thisExpr.GetComponent<ParametricExpression>())
            {
                param = thisExpr.GetComponent<ParametricExpression>();
                expressionSet = param.getExpSet();
                calcManager.RemoveExpressionSet(expressionSet);
                param.setTextColor(grayHide);
                expressionActions.GetComponent<ExpressionActions>().disableButtons();
                param.setButtonInputColor(grayHide);
                param.setElementQuadTex(quadHide);
                param.setActiveStatus(false);
            }

            if (expressions.getSelectedBody()) expressions.getSelectedBody().deselectCurrBody();
        }
        else            //SHOW
        {
            calcManager.AddExpressionSet(expressionSet);
            param.setActiveStatus(true);
            expressions.setSelectedExpr(thisExpr, thisBody);
            thisBody.selectBody();
            param.setTextColor(Color.black);
            param.setElementQuadTex(quadShow);
            param.setButtonInputColor(grayShow);
        }

        transform.GetComponent<Renderer>().material = (active) ? hideMat : showMat;
        active = !active;
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
