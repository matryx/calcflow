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
    Transform xButton;

    protected override void Start()
    {
        base.Start();
        calcManager = CalculatorManager._instance;
        expressions = GameObject.Find("ExpressionMenu").GetComponentInChildren<Expressions>();

        paramPanel = transform.parent.parent.Find("ParametrizationPanel");
        vecPanel = transform.parent.parent.Find("VectorFieldPanel");
        constPanel = transform.parent.parent.Find("ConstantPanel");

        thisScroll = expressions.getScroll("param");
        currPanel = paramPanel;

        joyStickAggregator = thisScroll.GetComponent<JoyStickAggregator>();
    }

    private void addForwarders(Transform obj)
    {
        JoyStickForwarder[] forwarders = obj.GetComponentsInChildren<JoyStickForwarder>();
        foreach (JoyStickForwarder j in forwarders)
        {
            joyStickAggregator.AddForwarder(j);
        }
    }

    //TODO: refactor
    protected override void ButtonEnterBehavior(GameObject other)
    {
        List<Transform> toAdd = new List<Transform>();
        ExpressionSet expressionSet = null;
        string panelType = "";

        if (paramPanel.gameObject.activeSelf)
        {
            panelType = "param";
            currPanel = paramPanel;

            GameObject param = Instantiate(Resources.Load("Expressions/ParametricExpression", typeof(GameObject))) as GameObject;
            param.GetComponent<ParametricExpression>().Initialize();

            expressionSet = param.GetComponent<ParametricExpression>().getExpSet();
            calcManager.AddExpressionSet(expressionSet);
            addForwarders(param.transform);

            foreach (Transform child in param.transform)
            {
                if (child.name == "ExpressionSet")
                {
                    foreach (Transform gchild in child)
                    {
                        param.GetComponent<ParametricExpression>().addExpression(gchild);

                        if (gchild.name == "Button_Xinput") xButton = gchild;

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
            //NOTE: COMMENTED OUT IN CURR IMPLEMENTATION 
            //expressions.setSelectedExpr(param.transform, xButton.GetComponentInChildren<ExpressionBody>());
        }
        else if (vecPanel.gameObject.activeSelf)
        {
            panelType = "vec";
            currPanel = vecPanel;

            GameObject vec = Instantiate(Resources.Load("Expressions/VectorFieldExpression", typeof(GameObject))) as GameObject;
            vec.GetComponent<VectorFieldExpression>().Initialize();
            expressionSet = vec.GetComponent<VectorFieldExpression>().getExpSet();
            calcManager.SetVecFieldES(expressionSet);
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

        if (expressions.getSelectedExpr())
        {
            Transform prevSep = expressions.getSelectedExpr().GetComponent<ParametricExpression>().getSeparator();
            thisScroll.addToScroll(toAdd, null, thisScroll.getIndex(prevSep) + 1);
        }
        else
        {
            //BUG: scoots scroll down when adding and not at top of page
            // - put on hold for now since it's a very specific case and need to merge with master
            thisScroll.addToScroll(toAdd, null, 0); 
        }

        if (xButton) xButton.GetComponentInChildren<ExpressionBody>().selectBody();

        xButton = null;
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update()
    {

    }
}
