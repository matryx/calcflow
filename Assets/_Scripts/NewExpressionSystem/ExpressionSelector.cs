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
    private ParametricManager paramManager;
    Transform xButton;

    protected override void Start()
    {
        base.Start();
        paramManager = ParametricManager._instance;
        expressions = GameObject.Find("ExpressionMenu").GetComponentInChildren<Expressions>();

        paramPanel = transform.parent.parent.Find("ParametrizationPanel");
        vecPanel = transform.parent.parent.Find("VectorFieldPanel");
        constPanel = transform.parent.parent.Find("ConstantPanel");

        thisScroll = expressions.getScroll(Expressions.ExpressionType.PARAMET);
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

    protected override void ButtonEnterBehavior(GameObject other)
    {
        List<Transform> toAdd = new List<Transform>();
        Expressions.ExpressionType panelType = Expressions.ExpressionType.PARAMET;

        if (paramPanel.gameObject.activeSelf)
        {
            panelType = Expressions.ExpressionType.PARAMET;
            currPanel = paramPanel;

            toAdd = createParametricExpression();
        }
        else if (vecPanel.gameObject.activeSelf)
        {
            panelType = Expressions.ExpressionType.VECFIELD;
            currPanel = vecPanel;

            toAdd = createVecExpression();
        }
        else if (constPanel.gameObject.activeSelf)
        {
            panelType = Expressions.ExpressionType.CONSTANT;
            currPanel = constPanel;

            toAdd = createConstant();
        }

        thisScroll = expressions.getScroll(panelType);

        if (expressions.getSelectedExpr())
        {
            Transform prevSep = expressions.getSelectedExpr().GetComponent<ParametricExpression>().getSeparator();
            thisScroll.addToScroll(toAdd, null, thisScroll.getIndex(prevSep) - 3);
        }
        else
        {
            thisScroll.addToScroll(toAdd, null, 0);
        }

        if (xButton)
        {
            xButton.GetComponentInChildren<ExpressionBody>().selectBody();
        }

        xButton = null;
    }

    private List<Transform> createParametricExpression()
    {
        List<Transform> paramComponents = new List<Transform>();
        ExpressionSet expressionSet = null;

        GameObject param = Instantiate(Resources.Load("Expressions/ParametricExpression", typeof(GameObject))) as GameObject;
        param.GetComponent<ParametricExpression>().Initialize();

        expressionSet = param.GetComponent<ParametricExpression>().getExpSet();
        if (!paramManager) paramManager = ParametricManager._instance;
        paramManager.AddExpressionSet(expressionSet);
        addForwarders(param.transform);

        paramSetUp(param.transform, paramComponents);

        GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        addForwarders(sep.transform);
        paramComponents.Add(sep.transform);

        param.GetComponent<ParametricExpression>().setSeparator(sep.transform);

        expressions.addExpr(param.transform);
        return paramComponents;
    }

    private void paramSetUp(Transform p, List<Transform> pComp)
    {
        foreach (Transform child in p)
        {
            if (child.name == "ExpressionSet")
            {
                foreach (Transform gchild in child)
                {
                    p.GetComponent<ParametricExpression>().addExpression(gchild);

                    if (gchild.name == "Button_Xinput") xButton = gchild;

                    gchild.GetComponentInChildren<ExpressionBody>().setExpressionParent(p);
                    gchild.GetComponentInChildren<ExpressionBody>().setPanel(currPanel);
                    pComp.Add(gchild);
                }
            }
        }
    }

    //BUG: vec field expressions added to scroll properly but cant be selected 
    //BUG: adding new expression while variable min or max is selected causes new exp to be inserted in the middle of existing expression (right after X)
    private List<Transform> createVecExpression()
    {
        List<Transform> vecComponents = new List<Transform>();
        ExpressionSet expressionSet = null;

        GameObject vec = Instantiate(Resources.Load("Expressions/VectorFieldExpression", typeof(GameObject))) as GameObject;
        vec.GetComponent<VectorFieldExpression>().Initialize();
        expressionSet = vec.GetComponent<VectorFieldExpression>().getExpSet();
        addForwarders(vec.transform);

        foreach (Transform child in vec.transform.Find("ExpressionSet"))
        {
            if (child.name == "Button_Xinput") xButton = child;
            vec.GetComponent<VectorFieldExpression>().addExpression(child);
            child.GetComponentInChildren<ExpressionBody>().setExpressionParent(vec.transform);
            child.GetComponentInChildren<ExpressionBody>().setPanel(currPanel);
            vecComponents.Add(child);
        }

        Transform var = createVariable(vec.transform);
        addForwarders(var.transform);
        vecComponents.Add(var.transform);

        GameObject sepVec = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        addForwarders(sepVec.transform);
        vecComponents.Add(sepVec.transform);

        expressions.addExpr(vec.transform);

        return vecComponents;
    }

    private Transform createVariable(Transform v)
    {
        GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
        var.gameObject.SetActive(true);

        var.GetComponentInChildren<ExpressionBody>().setExpressionParent(v);
        var.GetComponentInChildren<ExpressionBody>().setPanel(currPanel);
        var.transform.Find("VariableTitle").Find("Body").GetComponent<ExpressionBody>().setTitle("t");

        v.GetComponent<VectorFieldExpression>().getExpSet().AddRange("t");
        return var.transform;
    }

    private List<Transform> createConstant()
    {
        List<Transform> constComponents = new List<Transform>();

        GameObject cons = Instantiate(Resources.Load("Expressions/ConstantExpression", typeof(GameObject))) as GameObject;
        cons.GetComponent<Constant>().Initialize();
        cons.GetComponent<Constant>().addComponent(cons.transform.Find("Constant"));
        cons.GetComponentInChildren<ExpressionComponent>().setExpressionParent(cons.transform);
        cons.GetComponentInChildren<ExpressionComponent>().setPanel(currPanel);
        addForwarders(cons.transform);

        constComponents.Add(cons.transform.Find("Constant"));

        GameObject sepConst = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        addForwarders(sepConst.transform);
        constComponents.Add(sepConst.transform);

        expressions.addExpr(cons.transform);

        return constComponents;
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
