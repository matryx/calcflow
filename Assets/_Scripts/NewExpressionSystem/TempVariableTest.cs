using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TempVariableTest : QuickButton {
    public ParametricExpression param;

    protected override void Start()
    {
        base.Start();
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
        var.GetComponent<ExpressionComponent>().setExpressionParent(param.transform);
        var.GetComponent<ExpressionComponent>().setPanel(transform.parent.Find("ParametrizationPanel"));
        param.addVariable(var.transform);
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
