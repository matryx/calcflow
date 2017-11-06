using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionSelector : QuickButton
{
    Scroll thisScroll;
    Expressions expressions;
    JoyStickAggregator joyStickAggregator;
    Transform panel;

    protected override void Start()
    {
        base.Start();
        thisScroll = transform.parent.parent.GetComponentInChildren<Scroll>();
        panel = thisScroll.transform.parent;
        expressions = GameObject.Find("ExpressionMenu").GetComponentInChildren<Expressions>();
        joyStickAggregator = thisScroll.GetComponent<JoyStickAggregator>();

        thisScroll.addObject(transform.parent);
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
        List<Transform> toAdd = new List<Transform>();

        Transform fakeObj = new GameObject().transform;
        switch (transform.parent.name)
        {
            case "ConstantAdd":
                GameObject cons = Instantiate(Resources.Load("Expressions/ConstantExpression", typeof(GameObject))) as GameObject;
                cons.GetComponent<Constant>().Initialize();
                cons.GetComponent<Constant>().addComponent(cons.transform.Find("Constant"));
                cons.GetComponentInChildren<ExpressionComponent>().setExpressionParent(cons.transform);
                cons.GetComponentInChildren<ExpressionComponent>().setPanel(panel);
                addForwarders(cons.transform);

                toAdd.Add(cons.transform.Find("Constant"));
                expressions.addExpr(cons.transform);
                break;
            case "ParametrizationAdd":
                GameObject param = Instantiate(Resources.Load("Expressions/ParametricExpression", typeof(GameObject))) as GameObject;
                param.GetComponent<ParametricExpression>().Initialize();
                addForwarders(param.transform);

                foreach (Transform child in param.transform)
                {
                    if (child.name == "ExpressionSet")
                    {
                        foreach (Transform gchild in child)
                        {
                            param.GetComponent<ParametricExpression>().addExpression(gchild);
                            gchild.GetComponent<ExpressionComponent>().setExpressionParent(param.transform);
                            gchild.GetComponentInChildren<ExpressionComponent>().setPanel(panel);
                            toAdd.Add(gchild);
                        }
                    }
                }

                expressions.addExpr(param.transform);
                break;
            case "VectorFieldAdd":
                GameObject vec = Instantiate(Resources.Load("Expressions/VectorFieldExpression", typeof(GameObject))) as GameObject;
                vec.GetComponent<VectorFieldExpression>().Initialize();
                addForwarders(vec.transform);

                foreach (Transform child in vec.transform)
                {
                    switch (child.name)
                    {
                        case "ExpressionSet":
                            foreach (Transform gchild in child)
                            {
                                vec.GetComponent<VectorFieldExpression>().addExpression(gchild);
                                gchild.GetComponent<ExpressionComponent>().setExpressionParent(vec.transform);
                                gchild.GetComponentInChildren<ExpressionComponent>().setPanel(panel);
                                toAdd.Add(gchild);
                            }
                            break;
                        case "Variable":
                            vec.GetComponent<VectorFieldExpression>().setRange(child);
                            child.GetComponentInChildren<ExpressionComponent>().setExpressionParent(vec.transform);
                            child.GetComponentInChildren<ExpressionComponent>().setPanel(panel);
                            toAdd.Add(child);
                            break;
                    }
                }

                expressions.addExpr(vec.transform);
                break;
        }

        GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        addForwarders(sep.transform);
        toAdd.Add(sep.transform);
        thisScroll.addToIndex(-1, toAdd, fakeObj, true);
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update()
    {

    }
}
