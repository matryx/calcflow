using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ExpressionSelector : QuickButton
{
    Scroll thisScroll;
    Expressions expressions;
    JoyStickAggregator joyStickAggregator;
    Transform paramPanel, vecPanel, constPanel;
    private ParametricManager paramManager;
    private VecFieldManager vecFieldManager;
    Transform xButton;

    List<string> min = new List<string>();
    List<string> max = new List<string>();

    protected override void Start()
    {
        base.Start();
        paramManager = ParametricManager._instance;
        vecFieldManager = VecFieldManager._instance;
        expressions = GameObject.Find("ExpressionMenu").GetComponentInChildren<Expressions>();

        paramPanel = transform.parent.parent.Find("ParametrizationPanel");
        vecPanel = transform.parent.parent.Find("VectorFieldPanel");
        constPanel = transform.parent.parent.Find("ConstantPanel");
        thisScroll = expressions.getScroll(Expressions.ExpressionType.PARAMET);

        joyStickAggregator = thisScroll.GetComponent<JoyStickAggregator>();

        min.Add("-");
        min.Add("9");
        max.Add("9");
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
        List<Transform> toAdd = setToAdd();
        Expressions.ExpressionType panelType = setPanelType();

        thisScroll = expressions.getScroll(panelType);
        addExpressionToScroll(toAdd);

        if (xButton)
        {
            xButton.GetComponentInChildren<ExpressionBody>().selectBody();
            xButton = null;
        }
    }

    private List<Transform> setToAdd()
    {
        if (paramPanel.gameObject.activeSelf)
        {
            return createParametricExpression();
        }
        else if (vecPanel.gameObject.activeSelf)
        {
            return createVecExpression();
        }
        else if (constPanel.gameObject.activeSelf)
        {
            return createConstant();
        }
        else
        {
            return null;
        }
    }

    private Expressions.ExpressionType setPanelType()
    {
        if (paramPanel.gameObject.activeSelf)
        {
            return Expressions.ExpressionType.PARAMET;
        }
        else if (vecPanel.gameObject.activeSelf)
        {
            return Expressions.ExpressionType.VECFIELD;
        }
        else if (constPanel.gameObject.activeSelf)
        {
            return Expressions.ExpressionType.CONSTANT;
        }
        else
        {
            Debug.Log("<color=red>PANEL TYPE NOT SET</color>");
            return Expressions.ExpressionType.PARAMET;
        }
    }

    private void addExpressionToScroll(List<Transform> toAdd)
    {
        Transform prevXExpression = null;
        int startingIndex = 0;

        if (expressions.getSelectedExpr())
        {
            prevXExpression = expressions.getSelectedExpr().gameObject.GetInterface<ExpressionTabInterface>().getExpressionX();
            startingIndex = thisScroll.getIndex(prevXExpression) - 1;

            if (startingIndex < 0) startingIndex = 0;
        }

        thisScroll.addToScroll(toAdd, null, startingIndex);
    }

    private List<Transform> createParametricExpression()
    {
        List<Transform> paramComponents = new List<Transform>();
        ExpressionSet expressionSet = null;

        GameObject param = Instantiate(Resources.Load("Expressions/ParametricExpression", typeof(GameObject))) as GameObject;
        param.GetComponent<ParametricExpression>().Initialize();
        expressionSet = param.GetComponent<ParametricExpression>().getExpSet();
        addForwarders(param.transform);

        if (!paramManager) paramManager = ParametricManager._instance;
        paramManager.AddExpressionSet(expressionSet);

        paramSetUp(param.transform, paramComponents);

        GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        param.GetComponent<ParametricExpression>().setSeparator(sep.transform);
        addForwarders(sep.transform);
        paramComponents.Add(sep.transform);

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
                    if (gchild.name == "Button_Xinput")
                    {
                        p.GetComponent<ParametricExpression>().setExpressionX(gchild);
                        xButton = gchild;
                    }

                    p.GetComponent<ParametricExpression>().addExpression(gchild);
                    gchild.GetComponentInChildren<ExpressionBody>().setManager(ParametricManager._instance);
                    gchild.GetComponentInChildren<ExpressionBody>().setExpressionParent(p);
                    gchild.GetComponentInChildren<ExpressionBody>().setPanel(paramPanel);
                    pComp.Add(gchild);
                }
            }
        }
    }

    private List<Transform> createVecExpression()
    {
        List<Transform> vecComponents = new List<Transform>();
        ExpressionSet expressionSet = null;

        GameObject vec = Instantiate(Resources.Load("Expressions/VectorFieldExpression", typeof(GameObject))) as GameObject;
        vec.GetComponent<VectorFieldExpression>().Initialize();
        expressionSet = vec.GetComponent<VectorFieldExpression>().getExpSet();
        addForwarders(vec.transform);

        if (!vecFieldManager) vecFieldManager = VecFieldManager._instance;
        vecFieldManager.AddExpressionSet(expressionSet);
        vecSetUp(vec.transform, vecComponents);

        Transform var = createVariable(vec.transform);
        vecComponents.Add(var.transform);

        GameObject sepVec = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        vec.GetComponent<VectorFieldExpression>().setSeparator(sepVec.transform);
        addForwarders(sepVec.transform);
        vecComponents.Add(sepVec.transform);

        expressions.addExpr(vec.transform);

        return vecComponents;
    }

    private void vecSetUp(Transform vec, List<Transform> vComp)
    {
        foreach (Transform child in vec.transform.Find("ExpressionSet"))
        {
            if (child.name == "Button_Xinput")
            {
                vec.GetComponent<VectorFieldExpression>().setExpressionX(child);
                xButton = child;
            }

            vec.GetComponent<VectorFieldExpression>().addExpression(child);
            child.GetComponentInChildren<ExpressionBody>().setManager(VecFieldManager._instance);
            child.GetComponentInChildren<ExpressionBody>().setExpressionParent(vec.transform);
            child.GetComponentInChildren<ExpressionBody>().setPanel(vecPanel);
            vComp.Add(child);
        }
    }

    private Transform createVariable(Transform v)
    {
        GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
        var.gameObject.SetActive(true);
        var.transform.localScale = Vector3.one;
        var.transform.Find("Min").GetComponentInChildren<ExpressionBody>().setExpressionParent(v.transform);
        var.transform.Find("Max").GetComponentInChildren<ExpressionBody>().setExpressionParent(v.transform);
        var.transform.Find("Min").GetComponentInChildren<ExpressionBody>().setPanel(GameObject.Find("ExpressionMenu/VectorFieldPanel").transform);
        var.transform.Find("Max").GetComponentInChildren<ExpressionBody>().setPanel(GameObject.Find("ExpressionMenu/VectorFieldPanel").transform);
        var.transform.Find("VariableTitle").Find("Body").GetComponent<ExpressionBody>().setTitle("t");
        v.GetComponent<VectorFieldExpression>().setRange(var.transform);
        v.GetComponent<VectorFieldExpression>().getExpSet().AddRange("t");

        v.GetComponent<VectorFieldExpression>().getExpSet().AddRange("x", min, max);
        v.GetComponent<VectorFieldExpression>().getExpSet().AddRange("y", min, max);
        v.GetComponent<VectorFieldExpression>().getExpSet().AddRange("z", min, max);

        addForwarders(v.transform);
        return var.transform;
    }

    private List<Transform> createConstant()
    {
        List<Transform> constComponents = new List<Transform>();

        GameObject cons = Instantiate(Resources.Load("Expressions/ConstantExpression", typeof(GameObject))) as GameObject;
        cons.GetComponent<Constant>().Initialize();
        cons.GetComponent<Constant>().addComponent(cons.transform.Find("Constant"));
        cons.GetComponentInChildren<ExpressionComponent>().setExpressionParent(cons.transform);
        cons.GetComponentInChildren<ExpressionComponent>().setPanel(constPanel);
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
