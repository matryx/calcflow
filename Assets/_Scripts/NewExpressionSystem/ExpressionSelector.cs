using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionSelector : QuickButton
{
    Scroll thisScroll;
    Expressions expressions;
    JoyStickAggregator joyStickAggregator;

    protected override void Start()
    {
        base.Start();
        thisScroll = transform.parent.parent.GetComponentInChildren<Scroll>();
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
        List<Transform> emptyList = new List<Transform>();
        Transform fakeObj = new GameObject().transform;
        switch (transform.parent.name)
        {
            case "ConstantAdd":
                GameObject cons = Instantiate(Resources.Load("Expressions/ConstantExpression", typeof(GameObject))) as GameObject;
                cons.GetComponent<Constant>().Initialize();
                cons.GetComponent<Constant>().addComponent(cons.transform.Find("Constant"));
                cons.GetComponentInChildren<ExpressionComponent>().setExpressionParent(cons.transform);
                addForwarders(cons.transform);

                thisScroll.addToIndex(-1, emptyList, cons.transform.Find("Constant"), true);
                expressions.addExpr(cons.transform);
                break;
            case "ParametrizationAdd":
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
                            param.GetComponent<ParametricExpression>().addExpression(gchild);
                            gchild.GetComponent<ExpressionComponent>().setExpressionParent(param.transform);
                            gchildrenParam.Add(gchild);
                        }
                    }
                }

                thisScroll.addToIndex(-1, gchildrenParam, fakeObj, true);
                expressions.addExpr(param.transform);
                break;
            case "VectorFieldAdd":
                GameObject vec = Instantiate(Resources.Load("Expressions/VectorFieldExpression", typeof(GameObject))) as GameObject;
                vec.GetComponent<VectorFieldExpression>().Initialize();
                addForwarders(vec.transform);

                List<Transform> gchildrenVec = new List<Transform>();
                foreach (Transform child in vec.transform)
                {
                    switch (child.name)
                    {
                        case "ExpressionSet":
                            foreach (Transform gchild in child)
                            {
                                vec.GetComponent<VectorFieldExpression>().addExpression(gchild);
                                gchild.GetComponent<ExpressionComponent>().setExpressionParent(vec.transform);
                                gchildrenVec.Add(gchild);
                            }
                            break;
                        case "Variable":
                            vec.GetComponent<VectorFieldExpression>().setRange(child);
                            child.GetComponentInChildren<ExpressionComponent>().setExpressionParent(vec.transform);
                            gchildrenVec.Add(child);
                            break;
                    }
                }

                thisScroll.addToIndex(-1, gchildrenVec, fakeObj, true);
                expressions.addExpr(vec.transform);
                break;
        }

        GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        addForwarders(sep.transform);
        thisScroll.addToIndex(-1, emptyList, sep.transform, true);
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update()
    {

    }
}
