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
    Color gray;

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
        ColorUtility.TryParseHtmlString("#9E9E9EFF", out gray);
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        expressionSet = expressions.getSelectedExprSet();
        selectedExpression = expressions.getSelectedExpr();
        selectedBody = expressions.getSelectedBody();

        if (active)
        {
            calcManager.RemoveExpressionSet(expressionSet);
            selectedExpression.GetComponent<ParametricExpression>().setTextColor(gray);
            expressionActions.GetComponent<Renderer>().material.SetTexture("_MainTex", Resources.Load("Icons/finalEllipses_gray", typeof(Texture)) as Texture);
            expressionActions.GetComponent<ExpressionActions>().disableButtons();
            selectedExpression.GetComponent<ParametricExpression>().setButtonInputMaterial(Resources.Load("Icons/SeparatorGrey", typeof(Material)) as Material);
            selectedBody.deselectPrevBody();  
        }
        else
        {
            calcManager.AddExpressionSet(expressionSet);
            selectedExpression.GetComponent<ParametricExpression>().setTextColor(Color.black);
            expressionActions.GetComponent<Renderer>().material.SetTexture("_MainTex", Resources.Load("Icons/finalEllipses", typeof(Texture)) as Texture);
            selectedExpression.GetComponent<ParametricExpression>().setButtonInputMaterial(Resources.Load("Icons/WhiteUnlitFader", typeof(Material)) as Material);
            //TODO: select expression, component X
        }

        transform.GetComponent<Renderer>().material = (active) ? hideMat : showMat;
        active = !active;
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
