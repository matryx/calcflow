using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ExpressionCreator_UI : QuickButton
{
    public static ExpressionCreator_UI _instance;
    Expressions expressions;

    ParametricManager paramManager;
    VecFieldManager vecFieldManager;

    Scroll thisScroll;

    Transform xButton;
    Transform paramPanel, vecPanel;
    //Transform constPanel; 

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

        Expressions.ExpressionType panelType = GetPanelType();

        thisScroll = expressions.GetScroll(panelType);

        List<Transform> newExpression = CreateExpression();

        foreach (Transform component in newExpression) AddForwarders(component);

        AddExpressionToScroll(newExpression);

        if (xButton)
        {
            //enables typing 
            xButton.GetComponentInChildren<ExpressionBody>().SelectBody();
            xButton = null;
        }
    }

    List<Transform> CreateExpression()
    {
        if (paramPanel.gameObject.activeSelf)
        {
            return CreateParametricExpression_UI();
        }
        else if (vecPanel.gameObject.activeSelf)
        {
            return CreateVectorFieldExpression_UI();
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

    Expressions.ExpressionType GetPanelType()
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

    List<Transform> CreateParametricExpression_UI()
    {
        GameObject param = Instantiate(Resources.Load("Expressions/ParametricExpression", typeof(GameObject))) as GameObject;

        xButton = param.GetComponent<ParametricExpression>().GetExpressionX();

        List <Transform> paramComponents = param.GetComponent<ParametricExpression>().GetAllComponents();

        return paramComponents;
    } 

    List<Transform> CreateVectorFieldExpression_UI()
    {
        GameObject vec = Instantiate(Resources.Load("Expressions/VectorFieldExpression", typeof(GameObject))) as GameObject;

        xButton = vec.GetComponent<VectorFieldExpression>().GetExpressionX();

        List<Transform> vectorFieldComponents = vec.GetComponent<VectorFieldExpression>().GetAllComponents();

        return vectorFieldComponents;
    }

    #region Constant
    //private List<Transform> createConstant()
    //{
    //    List<Transform> constComponents = new List<Transform>();

    //    GameObject cons = Instantiate(Resources.Load("Expressions/ConstantExpression", typeof(GameObject))) as GameObject;
    //    cons.GetComponent<Constant>().Initialize();
    //    cons.GetComponent<Constant>().addComponent(cons.transform.Find("Constant"));
    //    cons.GetComponentInChildren<ExpressionComponent>().setExpressionParent(cons.transform);
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
