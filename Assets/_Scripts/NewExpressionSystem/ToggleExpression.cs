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
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (active)     //HIDE
        {
            if (thisExpr.GetComponent<ParametricExpression>())
            {
                param = thisExpr.GetComponent<ParametricExpression>();
                param.DisableExpression_UI();
                expressionSet = param.GetExpSet();
                calcManager.RemoveExpressionSet(expressionSet);
            }

            if (expressions.GetSelectedBody()) expressions.GetSelectedBody().DeselectCurrBody();
        }
        else            //SHOW
        {
            param.EnableExpression_UI();
            calcManager.AddExpressionSet(expressionSet);
            thisBody.SelectBody();
        }

        expressionActions.GetComponent<ExpressionActions>().DisableButtons();
        transform.GetComponent<Renderer>().material = (active) ? hideMat : showMat;
        active = !active;
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
