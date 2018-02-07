using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ToggleExpression : QuickButton
{
    Expressions expressions;
    CalculatorManager calcManager;
    ExpressionSet expressionSet;

    Material showMat, hideMat;

    bool active = true;
    
    protected override void Start()
    {
        base.Start();
        expressions = Expressions._instance;
        calcManager = CalculatorManager._instance;
        expressionSet = expressions.getSelectedExprSet();
        showMat = transform.GetComponent<Renderer>().material;
        hideMat = Resources.Load("Icons/HideMat", typeof(Material)) as Material;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        expressionSet = expressions.getSelectedExprSet();

        if (active)
        {
            calcManager.RemoveExpressionSet(expressionSet);
        }
        else
        {
            calcManager.AddExpressionSet(expressionSet);
        }

        transform.GetComponent<Renderer>().material = (active) ? hideMat : showMat;
        active = !active;
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
