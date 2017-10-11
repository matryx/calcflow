using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionSelector : QuickButton
{
    Scroll expressionScroll;
    Expressions expressions;
    //public AddExpression addExpr;

    protected override void Start()
    {
        base.Start();
        expressionScroll = GameObject.Find("ExpressionMenu").GetComponentInChildren<Scroll>();
        expressions = GameObject.Find("ExpressionMenu").GetComponentInChildren<Expressions>();
    }

    //TODO:
    // - test
    protected override void ButtonEnterBehavior(GameObject other)
    {
        switch (transform.parent.name)
        {
            case "Constant":
                GameObject cons = Instantiate(Resources.Load("Expressions/ConstantExpression", typeof(GameObject))) as GameObject;
                cons.GetComponent<Constant>().addComponent(cons.transform.Find("Constant"));

                //TODO: add to second to last index instead of end
                expressionScroll.addObject(cons.transform.Find("Constant"));
                expressions.addExpr(cons.transform);
                break;
            case "Parametrization":
                GameObject param = Instantiate(Resources.Load("Expressions/ParametricExpression", typeof(GameObject))) as GameObject;

                foreach (Transform child in param.transform)
                {
                    foreach (Transform gchild in child)
                    {
                        if (child.name == "ExpressionSet")
                        {
                            param.GetComponent<ParametricExpression>().addExpression(gchild);
                        }
                        else if (child.name == "Variables")
                        {
                            param.GetComponent<ParametricExpression>().addVariable(gchild);
                        }

                        //TODO: add to second to last index instead of end
                        expressionScroll.addObject(gchild);
                    }
                }

                expressions.addExpr(param.transform);
                break;
            case "VectorField":
                GameObject vec = Instantiate(Resources.Load("Expressions/VectorFieldExpression", typeof(GameObject))) as GameObject;

                foreach (Transform child in vec.transform)
                {
                    if (child.name == "Variable")
                    {
                        vec.GetComponent<VectorFieldExpression>().setRange(child);
                        //TODO: add to second to last index instead of end
                        expressionScroll.addObject(child);
                        continue;
                    }

                    foreach (Transform gchild in child)
                    {
                        vec.GetComponent<VectorFieldExpression>().addExpression(gchild);
                        //TODO: add to second to last index instead of end
                        expressionScroll.addObject(gchild);
                    }
                }

                expressions.addExpr(vec.transform);
                break;
        }

        GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        //TODO: add to second to last index instead of last
        expressionScroll.addObject(sep.transform);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        //addExpr.setAddActiveFalse(); //not needed anymore
    }

    void Update()
    {

    }
}
