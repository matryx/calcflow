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
        print("made var: " + var);
        param.addVariable(var.transform);
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    // Update is called once per frame
    void Update () {
		
	}
}
