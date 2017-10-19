using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionSelector : QuickButton
{
    Scroll expressionScroll;
    Expressions expressions;
    JoyStickAggregator joyStickAggregator;

    protected override void Start()
    {
        base.Start();
        expressionScroll = GameObject.Find("ExpressionMenu").GetComponentInChildren<Scroll>();
        expressions = GameObject.Find("ExpressionMenu").GetComponentInChildren<Expressions>();
        joyStickAggregator = expressionScroll.GetComponent<JoyStickAggregator>();
    }

    private void addForwarders(Transform obj)
    {
        JoyStickForwarder[] forwarders = obj.GetComponentsInChildren<JoyStickForwarder>();
        foreach (JoyStickForwarder j in forwarders)
        {
            joyStickAggregator.AddForwarder(j);
        }
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        List<Transform> emptyList = new List<Transform>();
        Transform fakeObj = new GameObject().transform;
        switch (transform.parent.name)
        {
            case "Constant":
                GameObject cons = Instantiate(Resources.Load("Expressions/ConstantExpression", typeof(GameObject))) as GameObject;
                cons.GetComponent<Constant>().Initialize();
                cons.GetComponent<Constant>().addComponent(cons.transform.Find("Constant"));
                addForwarders(cons.transform);

                expressionScroll.addToIndex(-1, emptyList, cons.transform.Find("Constant"), true);
                expressions.addExpr(cons.transform);
                break;
            case "Parametrization":
                GameObject param = Instantiate(Resources.Load("Expressions/ParametricExpression", typeof(GameObject))) as GameObject;
                param.GetComponent<ParametricExpression>().Initialize();
                addForwarders(param.transform);

                List<Transform> gchildrenParam = new List<Transform>();
                foreach (Transform child in param.transform)
                {
                    if (child.name == "ExpressionSet")
                    {
                        foreach (Transform gchild in child)
                        {
                            if (child.name == "ExpressionSet")
                            {
                                param.GetComponent<ParametricExpression>().addExpression(gchild);
                            }
                            gchildrenParam.Add(gchild);
                        }
                    }
                }

                expressionScroll.addToIndex(-1, gchildrenParam, fakeObj, true);
                expressions.addExpr(param.transform);
                break;
            case "VectorField":
                GameObject vec = Instantiate(Resources.Load("Expressions/VectorFieldExpression", typeof(GameObject))) as GameObject;
                vec.GetComponent<VectorFieldExpression>().Initialize();
                addForwarders(vec.transform);

                List<Transform> gchildrenVec = new List<Transform>();
                foreach (Transform child in vec.transform)
                {
                    if (child.name == "Variable")
                    {
                        vec.GetComponent<VectorFieldExpression>().setRange(child);
                        gchildrenVec.Add(child);
                        continue;
                    }

                    foreach (Transform gchild in child)
                    {
                        vec.GetComponent<VectorFieldExpression>().addExpression(gchild);
                        gchildrenVec.Add(gchild);
                    }

                }

                expressionScroll.addToIndex(-1, gchildrenVec, fakeObj, true);
                expressions.addExpr(vec.transform);
                break;
        }

        GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        addForwarders(sep.transform);
        expressionScroll.addToIndex(-1, emptyList, sep.transform, true);
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update()
    {

    }
}
