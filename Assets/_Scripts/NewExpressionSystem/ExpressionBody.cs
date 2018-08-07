using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ExpressionBody : QuickButton
{
    Expressions expression;

    Transform expressionParent;
    Transform panel;
    Transform feedBack;
    TMPro.TextMeshPro textInput;
    string title = "X";
    OutputManager outputManager;
    CalcInput calcInput;
    CalcOutput currVecFieldEq;

    ParametricExpression param;
    VectorFieldExpression vec;

    [SerializeField]
    CalculatorManager calcManager;

    Texture quadShow, quadHide;
    Color grayHide, grayShow;

    bool thisBodySelected = false;
    bool finishedScalingUp = true;
    bool finishedScalingDown = true;
    bool variable = false;

    Vector3 idleScale, selectedScale;

    IEnumerator scaleUp, scaleDown;

    void Awake()
    {
        calcManager = ParametricManager._instance;

        expression = GameObject.Find("Expressions").GetComponent<Expressions>();
        feedBack = transform.parent.Find("Feedback");

        if (transform.parent.parent.Find("VariableTitle")) variable = true;

        if (!variable) title = transform.parent.Find("Title").GetComponent<TMPro.TextMeshPro>().text.Substring(0, 1);

        if (transform.parent.Find("Text_Input"))
            textInput = transform.parent.Find("Text_Input").GetComponent<TMPro.TextMeshPro>();

        outputManager = expression.GetComponent<OutputManager>();
        calcInput = CalcInput._instance;

        selectedScale = (variable) ? new Vector3(0.7f, 0.04f, 0.002f) :
                                     new Vector3(4.3f, 0.04f, 0.002f);
        idleScale = new Vector3(0f, 0.04f, 0.002f);

        quadShow = Resources.Load("Icons/element", typeof(Texture2D)) as Texture;
        quadHide = Resources.Load("Icons/element_gray", typeof(Texture2D)) as Texture;
        ColorUtility.TryParseHtmlString("#9E9E9EFF", out grayShow);
        ColorUtility.TryParseHtmlString("#D4D4D4FF", out grayHide);
    }

    protected override void Start()
    {
        base.Start();
    }

    public void setManager(CalculatorManager cm)
    {
        calcManager = cm;
    }

    public CalculatorManager getManager()
    {
        return calcManager;
    }

    public void setExpressionParent(Transform p)
    {
        expressionParent = p;
    }

    public Transform getExpressionParent()
    {
        return expressionParent;
    }

    public void setPanel(Transform p)
    {
        panel = p;
    }

    public Transform getPanel()
    {
        return panel;
    }

    public Transform getFeedBack()
    {
        return feedBack;
    }

    public TMPro.TextMeshPro getTextInput()
    {
        return textInput;
    }

    public void setTitle(string t)
    {
        transform.parent.Find("Title").GetComponent<TMPro.TextMeshPro>().text = t;
        title = t;
    }

    public string getTitle()
    {
        return title;
    }

    public bool isVariable()
    {
        return variable;
    }

    //NOTE: sets selected expression and body to be null
    public void unSelect()
    {
        if (!finishedScalingUp)
        {
            StopCoroutine(scaleUp);
            finishedScalingUp = true;
        }

        scaleDown = ScaleTo(feedBack, feedBack.localScale, idleScale, 0.5f);
        StartCoroutine(scaleDown);
        finishedScalingDown = false;

        expression.setSelectedExpr(null, null);
        thisBodySelected = false;
    }

    public void deselectCurrBody()
    {
        if (expression == null) expression = GameObject.Find("Expressions").GetComponent<Expressions>();
        ExpressionBody selectedBody = expression.getSelectedBody();

        if (selectedBody)
        {
            TMPro.TextMeshPro oldTextInput = selectedBody.getTextInput();
            oldTextInput.text = oldTextInput.text.Replace("_", "");

            disableActionButtons(selectedBody);
            unSelect();
        }
    }

    //TODO: refactor to get rid of param and vec
    void disableActionButtons(ExpressionBody selectedBody)
    {
        if (selectedBody.getExpressionParent().GetComponent<ParametricExpression>())
        {
            param = selectedBody.getExpressionParent().GetComponent<ParametricExpression>();
            param.getExpActions().disableButtons();
        }
        else if (selectedBody.getExpressionParent().GetComponent<VectorFieldExpression>())
        {
            vec = selectedBody.getExpressionParent().GetComponent<VectorFieldExpression>();
            vec.getExpActions().disableButtons();
        }
    }

    private void selectBodyIfActive()
    {
        if (expressionParent.gameObject.GetInterface<ExpressionTabInterface>().getActiveStatus())
        {
            selectBody();
        }
    }

    public void deselectPrevBody()
    {
        if (expression == null) expression = GameObject.Find("Expressions").GetComponent<Expressions>();
        ExpressionBody selectedBody = expression.getSelectedBody();

        if (selectedBody)
        {
            TMPro.TextMeshPro oldTextInput = selectedBody.getTextInput();
            oldTextInput.text = oldTextInput.text.Replace("_", "");

            disableActionButtons(selectedBody);

            if (selectedBody.transform != transform)
            {
                selectedBody.unSelect();
            }

            //TODO: refactor so that it calls one function that does all this
            if (selectedBody.expressionParent != expressionParent && selectedBody.getManager() == VecFieldManager._instance)
            {
                selectedBody.expressionParent.gameObject.GetInterface<ExpressionTabInterface>().setTextColor(grayHide);
                selectedBody.expressionParent.gameObject.GetInterface<ExpressionTabInterface>().setButtonInputColor(grayHide);
                selectedBody.expressionParent.gameObject.GetInterface<ExpressionTabInterface>().setElementQuadTex(quadHide);
            }
        }
    }

    public void selectBody()
    {
        deselectPrevBody();
        expression.setSelectedExpr(expressionParent, this);
        calcManager.ChangeExpressionSet(expressionParent.gameObject.GetInterface<ExpressionTabInterface>().getExpSet());

        //TODO: refactor so that it calls one function that does all this
        if (calcManager == VecFieldManager._instance)
        {
            expressionParent.gameObject.GetInterface<ExpressionTabInterface>().setTextColor(Color.black);
            expressionParent.gameObject.GetInterface<ExpressionTabInterface>().setButtonInputColor(grayShow);
            expressionParent.gameObject.GetInterface<ExpressionTabInterface>().setElementQuadTex(quadShow);
        }

        if (variable)
        {
            title = transform.parent.parent.Find("VariableTitle").Find("Title").GetComponent<TMPro.TextMeshPro>().text;
            outputManager.HandleInput(transform.parent.name, title);
        }
        else
        {
            calcManager.SetOutput(calcManager.expressionSet.GetExpression(title));
        }

        if (!finishedScalingDown)
        {
            StopCoroutine(scaleDown);
            finishedScalingDown = true;
        }

        if (!feedBack) feedBack = transform.parent.Find("Feedback");
        scaleUp = ScaleTo(feedBack, feedBack.localScale, selectedScale, 0.3f);
        StartCoroutine(scaleUp);
        finishedScalingUp = false;
        thisBodySelected = true;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (thisBodySelected)
        {
            deselectCurrBody();
        }
        else
        {
            selectBodyIfActive();
        }
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    IEnumerator ScaleTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        if (end == selectedScale) obj.gameObject.SetActive(true);

        while (Time.time < startTime + overTime)
        {
            obj.localScale = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        obj.localScale = end;
        if (end == idleScale)
        {
            obj.gameObject.SetActive(false);
            finishedScalingDown = true;
        }
        else if (end == selectedScale)
        {
            finishedScalingUp = true;
        }
    }

    void OnDisable()
    {
        if (feedBack && feedBack.localScale == selectedScale)
        {
            feedBack.localScale = idleScale;
            feedBack.gameObject.SetActive(false);

            if (thisBodySelected)
            {
                if (calcManager == ParametricManager._instance)
                {
                    ExpressionBody selectedBody = expression.getSelectedBody();
                    TMPro.TextMeshPro oldTextInput = selectedBody.getTextInput();
                    oldTextInput.text = oldTextInput.text.Replace("_", "");
                    expression.setSelectedExpr(null, null);
                    thisBodySelected = false;
                }
                else
                {
                    currVecFieldEq = calcInput.currExpression;
                }

                calcInput.ChangeOutput(null, calcManager);
            }
        }
    }

    void OnEnable()
    {
        if (calcManager == VecFieldManager._instance && expression.getSelectedBody() == this)
        {
            feedBack.gameObject.SetActive(true);
            feedBack.localScale = selectedScale;
            calcInput.ChangeOutput(currVecFieldEq, calcManager);
        }
    }

    void Update() { }
}
