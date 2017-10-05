using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionSelector : QuickButton {
    Scroll expressionScroll;
    List<Transform> thisSelector;
    public enum ExpressionType { CONSTANT, PARAMET, VECFIELD }

    protected override void Start()
    {
        base.Start();
        expressionScroll = GameObject.Find("Expressions").GetComponentInChildren<Scroll>();
        thisSelector.Add(transform.parent.parent);
    }

    //TODO:
    // - need to handle re-adding of an expression selector after clicking a button
    //   (*) will solve this by removing current selector on exit behavior and having a button on each expression that lets you add another
    //       so all expressions will have both a delete and add new expresison button
    // - need Expressions to have a script for keeping track of expression clumps and which one is selected
    protected override void ButtonEnterBehavior(GameObject other)
    {
        switch (transform.name)
        {
            case "Constant":
                GameObject cons = Instantiate(Resources.Load("Expressions/Expression", typeof(GameObject))) as GameObject;
                expressionScroll.addObject(cons.transform);
                break;
            case "Parametrization":
                GameObject param1 = Instantiate(Resources.Load("Expressions/Expression", typeof(GameObject))) as GameObject;
                GameObject param2 = Instantiate(Resources.Load("Expressions/Expression", typeof(GameObject))) as GameObject;
                GameObject param3 = Instantiate(Resources.Load("Expressions/Expression", typeof(GameObject))) as GameObject;

                expressionScroll.addObject(param1.transform);
                expressionScroll.addObject(param2.transform);
                expressionScroll.addObject(param3.transform);
                break;
            case "VectorField":
                GameObject vec1 = Instantiate(Resources.Load("Expressions/Expression", typeof(GameObject))) as GameObject;
                GameObject vec2 = Instantiate(Resources.Load("Expressions/Expression", typeof(GameObject))) as GameObject;
                GameObject vec3 = Instantiate(Resources.Load("Expressions/Expression", typeof(GameObject))) as GameObject;
                GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;

                expressionScroll.addObject(vec1.transform);
                expressionScroll.addObject(vec2.transform);
                expressionScroll.addObject(vec3.transform);
                expressionScroll.addObject(var.transform);
                break;
        }

        GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        expressionScroll.addObject(sep.transform);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        expressionScroll.deleteObjects(thisSelector);
    }

    void Update () {
		
	}
}
