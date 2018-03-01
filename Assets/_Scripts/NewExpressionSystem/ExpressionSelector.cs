using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionSelector : QuickButton
{
    Scroll thisScroll;
    Expressions expressions;
    JoyStickAggregator joyStickAggregator;
    Transform currPanel;
    Transform paramPanel, vecPanel, constPanel;
    private CalculatorManager calcManager;


    protected override void Start()
    {
        base.Start();
        calcManager = CalculatorManager._instance;
        expressions = GameObject.Find("ExpressionMenu").GetComponentInChildren<Expressions>();
        //thisScroll = transform.parent.parent.GetComponentInChildren<Scroll>();

        paramPanel = transform.parent.parent.Find("ParametrizationPanel");
        vecPanel = transform.parent.parent.Find("VectorFieldPanel");
        constPanel = transform.parent.parent.Find("ConstantPanel");

        thisScroll = expressions.getScroll("param");
        currPanel = paramPanel;

        joyStickAggregator = thisScroll.GetComponent<JoyStickAggregator>();
        //thisScroll.addObject(transform.parent);

        #region old start
        //base.Start();
        //calcManager = CalculatorManager._instance;
        //thisScroll = transform.parent.parent.GetComponentInChildren<Scroll>();
        //panel = thisScroll.transform.parent;
        //expressions = GameObject.Find("ExpressionMenu").GetComponentInChildren<Expressions>();
        //joyStickAggregator = thisScroll.GetComponent<JoyStickAggregator>();
        //thisScroll.addObject(transform.parent);
        #endregion
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
        string panelType = "";

        if (paramPanel.gameObject.activeSelf)
        {
            panelType = "param";
            currPanel = paramPanel;

            GameObject param = Instantiate(Resources.Load("Expressions/ParametricExpression", typeof(GameObject))) as GameObject;
            param.GetComponent<ParametricExpression>().Initialize();
            calcManager.AddExpressionSet(param.GetComponent<ParametricExpression>().getExpSet());
            addForwarders(param.transform);

            foreach (Transform child in param.transform)
            {
                if (child.name == "ExpressionSet")
                {
                    foreach (Transform gchild in child)
                    {
                        param.GetComponent<ParametricExpression>().addExpression(gchild);
                        gchild.GetComponent<ExpressionComponent>().setExpressionParent(param.transform);
                        gchild.GetComponentInChildren<ExpressionComponent>().setPanel(currPanel);
                        toAdd.Add(gchild);
                    }
                }
            }

            GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
            addForwarders(sep.transform);
            toAdd.Add(sep.transform);

            param.GetComponent<ParametricExpression>().setSeparator(sep.transform);
            expressions.addExpr(param.transform);
        }
        else if (vecPanel.gameObject.activeSelf)
        {
            panelType = "vec";
            currPanel = vecPanel;

            GameObject vec = Instantiate(Resources.Load("Expressions/VectorFieldExpression", typeof(GameObject))) as GameObject;
            vec.GetComponent<VectorFieldExpression>().Initialize();
            calcManager.SetVecFieldES(vec.GetComponent<VectorFieldExpression>().getExpSet());
            //expressions.setSelectedExpr(vec.transform, null);
            addForwarders(vec.transform);

            foreach (Transform child in vec.transform.Find("ExpressionSet"))
            {
                vec.GetComponent<VectorFieldExpression>().addExpression(child);
                child.GetComponent<ExpressionComponent>().setExpressionParent(vec.transform);
                child.GetComponentInChildren<ExpressionComponent>().setPanel(currPanel);
                toAdd.Add(child);
            }

            GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
            var.GetComponent<ExpressionComponent>().setExpressionParent(vec.transform);
            var.GetComponent<ExpressionComponent>().setPanel(transform.parent.Find("ParametrizationPanel"));
            var.transform.Find("VariableTitle").Find("Body").GetComponent<ExpressionBody>().setTitle("t");
            vec.GetComponent<VectorFieldExpression>().getExpSet().AddRange("t");

            addForwarders(var.transform);
            toAdd.Add(var.transform);

            GameObject sepVec = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
            addForwarders(sepVec.transform);
            toAdd.Add(sepVec.transform);

            //set separator

            expressions.addExpr(vec.transform);
        }
        else if (constPanel.gameObject.activeSelf)
        {
            panelType = "cons";
            currPanel = constPanel;

            GameObject cons = Instantiate(Resources.Load("Expressions/ConstantExpression", typeof(GameObject))) as GameObject;
            cons.GetComponent<Constant>().Initialize();
            cons.GetComponent<Constant>().addComponent(cons.transform.Find("Constant"));
            cons.GetComponentInChildren<ExpressionComponent>().setExpressionParent(cons.transform);
            cons.GetComponentInChildren<ExpressionComponent>().setPanel(currPanel);
            addForwarders(cons.transform);

            toAdd.Add(cons.transform.Find("Constant"));

            GameObject sepConst = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
            addForwarders(sepConst.transform);
            toAdd.Add(sepConst.transform);

            expressions.addExpr(cons.transform);
        }

        thisScroll = expressions.getScroll(panelType);
        thisScroll.addToScroll(toAdd, null, 0);
        //thisScroll.addToIndex(0, toAdd, null, false);

        #region old switch

        //switch (transform.parent.name)
        //{
        //    case "ConstantAdd":
        //        GameObject cons = Instantiate(Resources.Load("Expressions/ConstantExpression", typeof(GameObject))) as GameObject;
        //        cons.GetComponent<Constant>().Initialize();
        //        cons.GetComponent<Constant>().addComponent(cons.transform.Find("Constant"));
        //        cons.GetComponentInChildren<ExpressionComponent>().setExpressionParent(cons.transform);
        //        cons.GetComponentInChildren<ExpressionComponent>().setPanel(panel);
        //        addForwarders(cons.transform);

        //        toAdd.Add(cons.transform.Find("Constant"));

        //        GameObject sepConst = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        //        addForwarders(sepConst.transform);
        //        toAdd.Add(sepConst.transform);

        //        expressions.addExpr(cons.transform);
        //        break;
        //    case "ParametrizationAdd":
        //        GameObject param = Instantiate(Resources.Load("Expressions/ParametricExpression", typeof(GameObject))) as GameObject;
        //        param.GetComponent<ParametricExpression>().Initialize();
        //        calcManager.AddExpressionSet(param.GetComponent<ParametricExpression>().getExpSet());
        //        addForwarders(param.transform);

        //        foreach (Transform child in param.transform)
        //        {
        //            if (child.name == "ExpressionSet")
        //            {
        //                foreach (Transform gchild in child)
        //                {
        //                    param.GetComponent<ParametricExpression>().addExpression(gchild);
        //                    gchild.GetComponent<ExpressionComponent>().setExpressionParent(param.transform);
        //                    gchild.GetComponentInChildren<ExpressionComponent>().setPanel(panel);
        //                    toAdd.Add(gchild);
        //                }
        //            }
        //        }

        //        GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        //        addForwarders(sep.transform);
        //        toAdd.Add(sep.transform);

        //        param.GetComponent<ParametricExpression>().setSeparator(sep.transform);
        //        expressions.addExpr(param.transform);
        //        break;
        //    case "VectorFieldAdd":
        //        GameObject vec = Instantiate(Resources.Load("Expressions/VectorFieldExpression", typeof(GameObject))) as GameObject;
        //        vec.GetComponent<VectorFieldExpression>().Initialize();
        //        calcManager.SetVecFieldES(vec.GetComponent<VectorFieldExpression>().getExpSet());
        //        //expressions.setSelectedExpr(vec.transform, null);
        //        addForwarders(vec.transform);

        //        foreach (Transform child in vec.transform.Find("ExpressionSet"))
        //        {
        //            vec.GetComponent<VectorFieldExpression>().addExpression(child);
        //            child.GetComponent<ExpressionComponent>().setExpressionParent(vec.transform);
        //            child.GetComponentInChildren<ExpressionComponent>().setPanel(panel);
        //            toAdd.Add(child);
        //        }

        //        GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
        //        var.GetComponent<ExpressionComponent>().setExpressionParent(vec.transform);
        //        var.GetComponent<ExpressionComponent>().setPanel(transform.parent.Find("ParametrizationPanel"));
        //        var.transform.Find("VariableTitle").Find("Body").GetComponent<ExpressionBody>().setTitle("t");
        //        vec.GetComponent<VectorFieldExpression>().getExpSet().AddRange("t");

        //        addForwarders(var.transform);
        //        toAdd.Add(var.transform);

        //        GameObject sepVec = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        //        addForwarders(sepVec.transform);
        //        toAdd.Add(sepVec.transform);

        //        //set separator

        //        expressions.addExpr(vec.transform);
        //        break;
        //}

        //GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        //addForwarders(sep.transform);
        //toAdd.Add(sep.transform);
        //thisScroll.addToIndex(-1, toAdd, null, true);

        #endregion
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update()
    {

    }
}
