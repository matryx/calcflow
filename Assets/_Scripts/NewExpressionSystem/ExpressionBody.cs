using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionBody : QuickButton
{
    Expressions expression;
    ExpressionComponent expComp;
    Transform feedBack;
    TMPro.TextMeshPro textInput;
    string title = "X";
    OutputManager outputManager;
    CalcInput calcInput;
    ParametricExpression param;
    CalculatorManager calcManager;

    private bool thisBodySelected = false;
    private bool finishedScaling = false;
    private bool retracting = false;
    private bool variable = false;

    private Vector3 idleScale, selectedScale;

    private IEnumerator scaleUp, scaleDown;
    private IEnumerator backToSelected, backToIdle;

    private void Awake()
    {
        calcManager = CalculatorManager._instance;
        expression = GameObject.Find("Expressions").GetComponent<Expressions>();
        feedBack = transform.parent.Find("Feedback");
        if (transform.parent.parent.Find("VariableTitle")) variable = true;

        if (!variable) title = transform.parent.Find("Title").GetComponent<TMPro.TextMeshPro>().text.Substring(0, 1);

        expComp = (variable) ? transform.parent.GetComponentInParent<ExpressionComponent>() :
                               transform.GetComponentInParent<ExpressionComponent>();

        if (transform.parent.Find("Text_Input"))
            textInput = transform.parent.Find("Text_Input").GetComponent<TMPro.TextMeshPro>();

        outputManager = expression.GetComponent<OutputManager>();
        calcInput = CalcInput._instance;

        selectedScale = (variable) ? new Vector3(0.7f, 0.04f, 0.002f) :
                                     new Vector3(4.3f, 0.04f, 0.002f);
        idleScale = new Vector3(0f, 0.04f, 0.002f);
    }

    protected override void Start()
    {
        base.Start();
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

    public void deselectCurrBody()
    {
        ExpressionBody selectedBody = expression.getSelectedBody();
        if (selectedBody)
        {
            TMPro.TextMeshPro oldTextInput = selectedBody.getTextInput();
            oldTextInput.text = oldTextInput.text.Replace("_", "");
            unSelect();
        }
    }

    public void deselectPrevBody()
    {
        ExpressionBody selectedBody = expression.getSelectedBody();
        if (selectedBody)
        {
            TMPro.TextMeshPro oldTextInput = selectedBody.getTextInput();
            oldTextInput.text = oldTextInput.text.Replace("_", "");

            if (selectedBody.transform != transform)
            {
                selectedBody.unSelect();
            }
        }
    }

    public void selectBody()
    {
        deselectPrevBody();

        if (expComp == null) expComp = transform.parent.GetComponentInParent<ExpressionComponent>();
        expression.setSelectedExpr(expComp.getExpressionParent(), this);

        if (!param) param = expComp.getExpressionParent().GetComponent<ParametricExpression>();
        calcManager.ChangeExpressionSet(param.getExpSet());

        if (variable)
        {
            title = transform.parent.parent.Find("VariableTitle").Find("Title").GetComponent<TMPro.TextMeshPro>().text;
            outputManager.HandleInput(transform.parent.name, title);
        }
        else
        {
            calcManager.SetOutput(calcManager.expressionSet.expressions[title]);
        }

        if (retracting && backToIdle != null)
        {
            StopCoroutine(backToIdle);
        }

        retracting = false;
        scaleUp = ScaleTo(feedBack, feedBack.localScale, selectedScale, 0.3f);
        StartCoroutine(scaleUp);
        finishedScaling = false;
        thisBodySelected = true;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        deselectPrevBody();

        if (thisBodySelected)
        {
            if (retracting && backToSelected != null)
            {
                StopCoroutine(backToSelected);
                retracting = false;
            }

            scaleDown = ScaleTo(feedBack, feedBack.localScale, idleScale, 0.3f);
            StartCoroutine(scaleDown);
            finishedScaling = false;
            expression.setSelectedExpr(null, null);

            thisBodySelected = false;
        }
        else
        {
            if (!param) param = expComp.getExpressionParent().GetComponent<ParametricExpression>();
            if (param.getActiveStatus()) selectBody();
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
            finishedScaling = true;
        }
    }

    //NOTE: sets selected expression and body to be null
    public void unSelect()
    {
        if (!finishedScaling && scaleUp != null) StopCoroutine(scaleUp);
        backToIdle = ScaleTo(feedBack, feedBack.localScale, idleScale, 0.5f);
        StartCoroutine(backToIdle);
        retracting = true;

        expression.setSelectedExpr(null, null);
        thisBodySelected = false;
    }

    //BUG: null feedback when adding a new variable that's offscreen
    private void OnDisable()
    {
        if (feedBack && feedBack.localScale == selectedScale)
        {
            if (transform.parent.name.Equals("Delete")) print("ON DISABLE");

            feedBack.localScale = idleScale;
            feedBack.gameObject.SetActive(false);

            if (thisBodySelected)
            {
                ExpressionBody selectedBody = expression.getSelectedBody();
                TMPro.TextMeshPro oldTextInput = selectedBody.getTextInput();
                oldTextInput.text = oldTextInput.text.Replace("_", "");
                expression.setSelectedExpr(null, null);
                thisBodySelected = false;
                calcInput.ChangeOutput(null);
            }
        }
    }

    void Update() { }
}
