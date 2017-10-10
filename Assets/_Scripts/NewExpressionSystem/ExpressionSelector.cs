using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionSelector : QuickButton {
    Scroll expressionScroll;
    Expressions expressionHandler;
    public AddExpression addExpr;

    protected override void Start()
    {
        base.Start();
        expressionScroll = GameObject.Find("Expressions").GetComponentInChildren<Scroll>();
        expressionHandler = expressionScroll.GetComponentInParent<Expressions>();
    }

    //TODO:
    // - need Expressions to have a script for keeping track of expression clumps and which one is selected
    // - decide how to handle parenting and separate parts
    protected override void ButtonEnterBehavior(GameObject other)
    {
        switch (transform.name)
        {
            case "Constant":
                GameObject cons = Instantiate(Resources.Load("Expressions/Constant", typeof(GameObject))) as GameObject;
                cons.GetComponent<Constant>().addComponent(cons.transform);
                expressionScroll.addObject(cons.transform);
                break;
            case "Parametrization":
                //Transform paramParent = new GameObject().transform;
                //paramParent.name = "Parametrization";
                //paramParent.localEulerAngles = Vector3.zero;
                //paramParent.localScale = Vector3.one;

                GameObject param = Instantiate(Resources.Load("Expressions/ExpressionSet", typeof(GameObject))) as GameObject;
                param.AddComponent<ParametricExpression>();
                param.GetComponent<ParametricExpression>().addComponent(param.transform);

                //param.transform.SetParent(paramParent);
                
                foreach (Transform child in param.transform)
                {
                    expressionScroll.addObject(child);
                }
                break;
            case "VectorField":
                //Transform vecParent = new GameObject().transform;
                //vecParent.name = "VectorField";
                //vecParent.localEulerAngles = Vector3.zero;
                //vecParent.localScale = Vector3.one;

                GameObject vec = Instantiate(Resources.Load("Expressions/Expression", typeof(GameObject))) as GameObject;
                vec.AddComponent<VectorFieldExpression>();
                vec.GetComponent<VectorFieldExpression>().addComponent(vec.transform);
                //vec.transform.SetParent(vecParent);

                foreach (Transform child in vec.transform)
                {
                    expressionScroll.addObject(child);
                }

                GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
                vec.GetComponent<VectorFieldExpression>().addComponent(var.transform);

                //var.transform.SetParent(vecParent);
                expressionScroll.addObject(var.transform);
                break;
        }

        GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        expressionScroll.addObject(sep.transform);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        addExpr.setAddActiveFalse();
    }

    void Update () {
		
	}
}
