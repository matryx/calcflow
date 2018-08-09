using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ToggleExpression : QuickButton
{
    Expressions expressions;
    ExpressionBody thisBody;
    ExpressionSet expressionSet;

    ParametricManager calcManager;
    ParametricExpression param;

    Transform expressionActions;
    Transform thisExpr;

    Material showMat, hideMat;
    Texture quadShow, quadHide;
    Color grayHide, grayShow;

    bool active = true;
    
    protected override void Start()
    {
        base.Start();
        expressions = Expressions._instance;
        calcManager = ParametricManager._instance;
        expressionSet = expressions.GetSelectedExprSet();

        thisBody = transform.parent.parent.parent.Find("Button_Input").GetComponent<ExpressionBody>();
        thisExpr = thisBody.GetExpressionParent();

        expressionActions = transform.parent.parent.Find("Body");

        showMat = transform.GetComponent<Renderer>().material;
        hideMat = Resources.Load("Icons/HideMat", typeof(Material)) as Material;

        quadShow = Resources.Load("Icons/element", typeof(Texture2D)) as Texture;
        quadHide = Resources.Load("Icons/element_gray", typeof(Texture2D)) as Texture;
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
                expressionSet = param.GetExpSet();
                calcManager.RemoveExpressionSet(expressionSet);
                param.SetTextColor(grayHide);
                param.SetButtonInputColor(grayHide);
                param.SetElementQuadTex(quadHide);
                param.SetActiveStatus(false);
            }

            if (expressions.GetSelectedBody()) expressions.GetSelectedBody().DeselectCurrBody();
        }
        else            //SHOW
        {
            calcManager.AddExpressionSet(expressionSet);
            param.SetActiveStatus(true);
            thisBody.SelectBody();
            param.SetTextColor(Color.black);
            param.SetElementQuadTex(quadShow);
            param.SetButtonInputColor(grayShow);
        }

        expressionActions.GetComponent<ExpressionActions>().disableButtons();
        transform.GetComponent<Renderer>().material = (active) ? hideMat : showMat;
        active = !active;
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
