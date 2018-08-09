using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ExpressionBody : QuickButton
{
    Expressions expression;

    CalcInput calcInput;
    CalcOutput currVecFieldEq;

    [SerializeField]
    CalculatorManager calcManager;
    OutputManager outputManager;

    ParametricExpression param;
    VectorFieldExpression vec;

    Transform expressionParent;
    Transform panel;
    Transform feedBack;

    TMPro.TextMeshPro textInput;
    Texture quadShow, quadHide;
    string title = "X";
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

    public void SetManager(CalculatorManager cm)
    {
        calcManager = cm;
    }

    public CalculatorManager GetManager()
    {
        return calcManager;
    }

    public void SetExpressionParent(Transform p)
    {
        expressionParent = p;
    }

    public Transform GetExpressionParent()
    {
        return expressionParent;
    }

    public void SetPanel(Transform p)
    {
        panel = p;
    }

    public Transform GetPanel()
    {
        return panel;
    }

    public Transform GetFeedBack()
    {
        return feedBack;
    }

    public TMPro.TextMeshPro GetTextInput()
    {
        return textInput;
    }

    public void SetTitle(string t)
    {
        transform.parent.Find("Title").GetComponent<TMPro.TextMeshPro>().text = t;
        title = t;
    }

    public string GetTitle()
    {
        return title;
    }

    public bool IsVariable()
    {
        return variable;
    }

    public void DeselectCurrBody()
    {
        if (expression == null) expression = GameObject.Find("Expressions").GetComponent<Expressions>();
        ExpressionBody selectedBody = expression.GetSelectedBody();

        if (selectedBody)
        {
            TMPro.TextMeshPro oldTextInput = selectedBody.GetTextInput();
            oldTextInput.text = oldTextInput.text.Replace("_", "");

            DisableActionButtons(selectedBody);
            UnSelect();
        }
    }

    public void DeselectPrevBody()
    {
        if (expression == null) expression = GameObject.Find("Expressions").GetComponent<Expressions>();
        ExpressionBody selectedBody = expression.GetSelectedBody();

        if (selectedBody)
        {
            TMPro.TextMeshPro oldTextInput = selectedBody.GetTextInput();
            oldTextInput.text = oldTextInput.text.Replace("_", "");

            DisableActionButtons(selectedBody);

            if (selectedBody.transform != transform)
            {
                selectedBody.UnSelect();
            }

            if (selectedBody.expressionParent != expressionParent && selectedBody.GetManager() == VecFieldManager._instance)
            {
                HideUI(selectedBody);
            }
        }
    }

    void DisableActionButtons(ExpressionBody selectedBody)
    {
        selectedBody.GetExpressionParent().gameObject.GetInterface<ExpressionTabInterface>().GetExpActions().disableButtons();
    }

    //NOTE: sets selected expression and body to be null
    public void UnSelect()
    {
        if (!finishedScalingUp)
        {
            StopCoroutine(scaleUp);
            finishedScalingUp = true;
        }

        scaleDown = ScaleTo(feedBack, feedBack.localScale, idleScale, 0.5f);
        StartCoroutine(scaleDown);
        finishedScalingDown = false;

        expression.SetSelectedExpr(null, null);
        thisBodySelected = false;
    }

    public void SelectBody()
    {
        DeselectPrevBody();
        expression.SetSelectedExpr(expressionParent, this);
        calcManager.ChangeExpressionSet(expressionParent.gameObject.GetInterface<ExpressionTabInterface>().GetExpSet());

        if (calcManager == VecFieldManager._instance)
        {
            ShowUI();
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

    void HideUI(ExpressionBody selectedBody)
    {
        selectedBody.expressionParent.gameObject.GetInterface<ExpressionTabInterface>().SetTextColor(grayHide);
        selectedBody.expressionParent.gameObject.GetInterface<ExpressionTabInterface>().SetButtonInputColor(grayHide);
        selectedBody.expressionParent.gameObject.GetInterface<ExpressionTabInterface>().SetElementQuadTex(quadHide);
    }

    void ShowUI()
    {
        expressionParent.gameObject.GetInterface<ExpressionTabInterface>().SetTextColor(Color.black);
        expressionParent.gameObject.GetInterface<ExpressionTabInterface>().SetButtonInputColor(grayShow);
        expressionParent.gameObject.GetInterface<ExpressionTabInterface>().SetElementQuadTex(quadShow);
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (thisBodySelected)
        {
            DeselectCurrBody();
        }
        else
        {
            SelectBodyIfActive();
        }
    }

    private void SelectBodyIfActive()
    {
        if (expressionParent.gameObject.GetInterface<ExpressionTabInterface>().GetActiveStatus())
        {
            SelectBody();
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
        if (feedBack && feedBack.gameObject.activeSelf)
        {
            feedBack.localScale = idleScale;
            feedBack.gameObject.SetActive(false);

            if (thisBodySelected)
            {
                hideThisBody();
                calcInput.ChangeOutput(null, calcManager);
            }
        }
    }

    void hideThisBody()
    {
        if (calcManager == ParametricManager._instance)
        {
            ExpressionBody selectedBody = expression.GetSelectedBody();
            TMPro.TextMeshPro oldTextInput = selectedBody.GetTextInput();
            oldTextInput.text = oldTextInput.text.Replace("_", "");
            expression.SetSelectedExpr(null, null);
            thisBodySelected = false;
        }
        else
        {
            currVecFieldEq = calcInput.currExpression;
        }
    }

    void OnEnable()
    {
        if (calcManager == VecFieldManager._instance && expression.GetSelectedBody() == this)
        {
            feedBack.gameObject.SetActive(true);
            feedBack.localScale = selectedScale;
            calcInput.ChangeOutput(currVecFieldEq, calcManager);
        }
    }

    void Update() { }
}
