using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ExpressionSelector : QuickButton
{
    public static ExpressionSelector _instance;
    Expressions expressions;

    ParametricManager paramManager;
    VecFieldManager vecFieldManager;

    Scroll thisScroll;

    Transform xButton;
    Transform paramPanel, vecPanel;
    //Transform constPanel;

    List<string> min;
    List<string> max;

    bool initialized = false;

    void Awake()
    {
        _instance = this;
    }

    protected override void Start()
    {
        base.Start();
        if (!initialized)
        {
            Initialize();
        }
    }

    void Initialize()
    {
        paramManager = ParametricManager._instance;
        vecFieldManager = VecFieldManager._instance;
        expressions = Expressions._instance;

        paramPanel = transform.parent.parent.Find("ParametrizationPanel");
        vecPanel = transform.parent.parent.Find("VectorFieldPanel");
        //constPanel = transform.parent.parent.Find("ConstantPanel");
        thisScroll = expressions.GetScroll(Expressions.ExpressionType.PARAMET);

        min = new List<string> { "-", "9" };
        max = new List<string> { "9" };
        initialized = true;
    }

    void AddForwarders(Transform obj)
    {
        JoyStickAggregator joyStickAggregator = thisScroll.GetComponent<JoyStickAggregator>();
        JoyStickForwarder[] forwarders = obj.GetComponentsInChildren<JoyStickForwarder>(true);

        foreach (JoyStickForwarder j in forwarders)
        {
            joyStickAggregator.AddForwarder(j);
        }
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        GenerateNewExpression();
    }

    public void GenerateNewExpression()
    {
        if (!initialized)
        {
            Initialize();
        }
        Expressions.ExpressionType panelType = SetPanelType();
        thisScroll = expressions.GetScroll(panelType);

        List<Transform> toAdd = SetToAdd();
        AddExpressionToScroll(toAdd);

        if (xButton)
        {
            //enables typing 
            xButton.GetComponentInChildren<ExpressionBody>().SelectBody();
            xButton = null;
        }
    }

    List<Transform> SetToAdd()
    {
        if (paramPanel.gameObject.activeSelf)
        {
            return CreateParametricExpression();
        }
        else if (vecPanel.gameObject.activeSelf)
        {
            return CreateVectorFieldExpression();
        }
        //else if (constPanel.gameObject.activeSelf)
        //{
        //    return createConstant();
        //}
        else
        {
            return null;
        }
    }

    Expressions.ExpressionType SetPanelType()
    {
        if (paramPanel == null) paramPanel = transform.parent.parent.Find("ParametrizationPanel");
        if (vecPanel == null) vecPanel = transform.parent.parent.Find("VectorFieldPanel");

        if (paramPanel.gameObject.activeSelf)
        {
            return Expressions.ExpressionType.PARAMET;
        }
        else if (vecPanel.gameObject.activeSelf)
        {
            return Expressions.ExpressionType.VECFIELD;
        }
        //else if (constPanel.gameObject.activeSelf)
        //{
        //    return Expressions.ExpressionType.CONSTANT;
        //}
        else
        {
            Debug.Log("<color=red>PANEL TYPE NOT SET</color>");
            return Expressions.ExpressionType.PARAMET;
        }
    }

    void AddExpressionToScroll(List<Transform> toAdd)
    {
        int lowestVisIndex = thisScroll.GetLowestVisibleIndex();
        int startingIndex = lowestVisIndex;

        if (expressions.GetSelectedExpr())
        {
            Transform prevXExpression = expressions.GetSelectedExpr().gameObject.GetInterface<ExpressionTabInterface>().GetExpressionX();
            startingIndex = thisScroll.GetIndex(prevXExpression);

            if (startingIndex < lowestVisIndex)
            {
                Transform prevSeparator = expressions.GetSelectedExpr().gameObject.GetInterface<ExpressionTabInterface>().GetSeparator();
                startingIndex = thisScroll.GetIndex(prevSeparator) + 1;
            }
        }
        else
        {
            if (thisScroll.GetScrollObjectCount() > 0 && !thisScroll.GetObj(startingIndex).name.Contains("Button_X"))
            {
                while (!thisScroll.GetObj(startingIndex).name.Contains("Sep"))
                {
                    startingIndex += 1;
                }

                startingIndex += 1;
            }
        }

        thisScroll.AddToScroll(toAdd, null, startingIndex);
    }

    List<Transform> CreateParametricExpression()
    {
        List<Transform> paramComponents = new List<Transform>();

        GameObject param = Instantiate(Resources.Load("Expressions/ParametricExpression", typeof(GameObject))) as GameObject;
        param.GetComponent<ParametricExpression>().Initialize();
        ExpressionSet expressionSet = param.GetComponent<ParametricExpression>().GetExpSet();

        if (!paramManager) paramManager = ParametricManager._instance;
        paramManager.AddExpressionSet(expressionSet);

        ParametricSetUp(param.transform, paramComponents);

        GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        param.GetComponent<ParametricExpression>().SetSeparator(sep.transform);
        AddForwarders(sep.transform);
        paramComponents.Add(sep.transform);

        expressions.AddExpr(param.transform);

        return paramComponents;
    }

    void ParametricSetUp(Transform p, List<Transform> pComp)
    {
        foreach (Transform child in p)
        {
            if (child.name == "ExpressionSet")
            {
                foreach (Transform gchild in child)
                {
                    if (gchild.name == "Button_Xinput")
                    {
                        p.GetComponent<ParametricExpression>().SetExpressionX(gchild);
                        xButton = gchild;
                    }

                    p.GetComponent<ParametricExpression>().AddExpression(gchild);
                    gchild.GetComponentInChildren<ExpressionBody>().SetManager(ParametricManager._instance);
                    gchild.GetComponentInChildren<ExpressionBody>().SetExpressionParent(p);
                    gchild.GetComponentInChildren<ExpressionBody>().SetPanel(paramPanel);
                    AddForwarders(gchild);
                    pComp.Add(gchild);
                }
            }
        }
    }

    List<Transform> CreateVectorFieldExpression()
    {
        List<Transform> vecComponents = new List<Transform>();
        ExpressionSet expressionSet = null;

        GameObject vec = Instantiate(Resources.Load("Expressions/VectorFieldExpression", typeof(GameObject))) as GameObject;
        vec.GetComponent<VectorFieldExpression>().Initialize();
        expressionSet = vec.GetComponent<VectorFieldExpression>().GetExpSet();

        if (!vecFieldManager) vecFieldManager = VecFieldManager._instance;
        vecFieldManager.AddExpressionSet(expressionSet);
        VectorFieldSetUp(vec.transform, vecComponents);

        Transform var = CreateVariable(vec.transform);
        AddForwarders(var.transform);
        vecComponents.Add(var.transform);

        GameObject sepVec = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        vec.GetComponent<VectorFieldExpression>().SetSeparator(sepVec.transform);
        AddForwarders(sepVec.transform);
        vecComponents.Add(sepVec.transform);

        expressions.AddExpr(vec.transform);

        return vecComponents;
    }

    void VectorFieldSetUp(Transform vec, List<Transform> vComp)
    {
        foreach (Transform child in vec.transform.Find("ExpressionSet"))
        {
            if (child.name == "Button_Xinput")
            {
                vec.GetComponent<VectorFieldExpression>().SetExpressionX(child);
                xButton = child;
            }

            vec.GetComponent<VectorFieldExpression>().AddExpression(child);
            child.GetComponentInChildren<ExpressionBody>().SetManager(VecFieldManager._instance);
            child.GetComponentInChildren<ExpressionBody>().SetExpressionParent(vec.transform);
            child.GetComponentInChildren<ExpressionBody>().SetPanel(vecPanel);
            AddForwarders(child);
            vComp.Add(child);
        }
    }

    Transform CreateVariable(Transform v)
    {
        GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
        var.gameObject.SetActive(true);
        var.transform.localScale = Vector3.one;
        var.transform.Find("Min").GetComponentInChildren<ExpressionBody>().SetManager(VecFieldManager._instance);
        var.transform.Find("Max").GetComponentInChildren<ExpressionBody>().SetManager(VecFieldManager._instance);
        var.transform.Find("Min").GetComponentInChildren<ExpressionBody>().SetExpressionParent(v.transform);
        var.transform.Find("Max").GetComponentInChildren<ExpressionBody>().SetExpressionParent(v.transform);
        var.transform.Find("Min").GetComponentInChildren<ExpressionBody>().SetPanel(GameObject.Find("ExpressionMenu/VectorFieldPanel").transform);
        var.transform.Find("Max").GetComponentInChildren<ExpressionBody>().SetPanel(GameObject.Find("ExpressionMenu/VectorFieldPanel").transform);
        var.transform.Find("VariableTitle").Find("Body").GetComponent<ExpressionBody>().SetTitle("t");
        v.GetComponent<VectorFieldExpression>().SetRange(var.transform);
        v.GetComponent<VectorFieldExpression>().GetExpSet().AddRange("t");

        v.GetComponent<VectorFieldExpression>().GetExpSet().AddRange("x", min, max);
        v.GetComponent<VectorFieldExpression>().GetExpSet().AddRange("y", min, max);
        v.GetComponent<VectorFieldExpression>().GetExpSet().AddRange("z", min, max);

        return var.transform;
    }

    #region Constant
    //private List<Transform> createConstant()
    //{
    //    List<Transform> constComponents = new List<Transform>();

    //    GameObject cons = Instantiate(Resources.Load("Expressions/ConstantExpression", typeof(GameObject))) as GameObject;
    //    cons.GetComponent<Constant>().Initialize();
    //    cons.GetComponent<Constant>().addComponent(cons.transform.Find("Constant"));
    //    cons.GetComponentInChildren<ExpressionComponent>().setExpressionParent(cons.transform);
    //    cons.GetComponentInChildren<ExpressionComponent>().setPanel(constPanel);
    //    addForwarders(cons.transform);

    //    constComponents.Add(cons.transform.Find("Constant"));

    //    GameObject sepConst = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
    //    addForwarders(sepConst.transform);
    //    constComponents.Add(sepConst.transform);

    //    expressions.addExpr(cons.transform);

    //    return constComponents;
    //}
    #endregion

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
