using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionSelector : QuickButton {
    Scroll expressionScroll;
    Expressions expressionHandler;
    List<Transform> thisSelector;

    protected override void Start()
    {
        base.Start();
        expressionScroll = GameObject.Find("Expressions").GetComponentInChildren<Scroll>();
        expressionHandler = expressionScroll.GetComponentInParent<Expressions>();
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
                GameObject cons = Instantiate(Resources.Load("Expressions/Constant", typeof(GameObject))) as GameObject;
                expressionScroll.addObject(cons.transform);
                break;
            case "Parametrization":
                GameObject param = Instantiate(Resources.Load("Expressions/Expression", typeof(GameObject))) as GameObject;
                
                foreach (Transform child in param.transform)
                {
                    expressionScroll.addObject(child);
                }
                break;
            case "VectorField":
                GameObject vec = Instantiate(Resources.Load("Expressions/Expression", typeof(GameObject))) as GameObject;

                foreach (Transform child in vec.transform)
                {
                    expressionScroll.addObject(child);
                }

                GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
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
