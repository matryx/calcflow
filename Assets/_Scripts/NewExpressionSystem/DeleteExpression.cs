using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DeleteExpression : QuickButton {
    Expressions expressions;

	// Use this for initialization
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

    // Update is called once per frame
    void Update () {
		
	}
}
