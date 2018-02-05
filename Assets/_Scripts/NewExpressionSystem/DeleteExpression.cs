using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DeleteExpression : QuickButton {
    Expressions expressions;

    protected override void Start()
    {
        base.Start();
        expressions = Expressions._instance;
	}

    protected override void ButtonEnterBehavior(GameObject other)
    {
        expressions.deleteExpression();
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
