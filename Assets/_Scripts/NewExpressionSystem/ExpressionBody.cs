using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionBody : QuickButton {
    Expressions expression;

    protected override void Start()
    {
        base.Start();
        //BUG: returning null
        expression = GameObject.Find("Expression").GetComponent<Expressions>();
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        ExpressionComponent expComp = transform.GetComponentInParent<ExpressionComponent>();
        if (expComp == null) {
            expComp = transform.parent.GetComponentInParent<ExpressionComponent>();
        }

        expression.setSelectedExpr(expComp.getExpressionParent());
    }

    protected override void ButtonExitBehavior(GameObject other) { }


    void Update () { }
}
