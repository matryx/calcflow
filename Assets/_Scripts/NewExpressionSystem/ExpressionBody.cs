using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionBody : QuickButton
{
    Expressions expression;
    ExpressionComponent expComp;
    Transform feedBack;
    TMPro.TextMeshPro textInput;
    string title;
    OutputManager outputManager;
    CalcInput calcInput;

    private bool thisBodyActive = false;
    private bool finishedScaling = false;
    private bool retracting = false;
    private bool variable = false;

    private Vector3 idleScale, selectedScale;

    private IEnumerator scaleUp, scaleDown;
    private IEnumerator backToSelected, backToIdle;

    //private void Awake()
    //{
    //    if (transform.parent.parent.Find("VariableTitle")) variable = true;

    //    title = (variable) ? transform.parent.parent.Find("VariableTitle").Find("Title").GetComponent<TMPro.TextMeshPro>().text :
    //                       transform.parent.Find("Title").GetComponent<TMPro.TextMeshPro>().text.Substring(0, 1);
    //}

    protected override void Start()
    {
        base.Start();
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
                                     new Vector3(4.569993f, 0.04f, 0.002f);
        idleScale = new Vector3(0f, 0.04f, 0.002f);
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

    protected override void ButtonEnterBehavior(GameObject other)
    {
        ExpressionBody selectedBody = expression.getSelectedBody();
        if (selectedBody && selectedBody.transform != transform)
        {
            TMPro.TextMeshPro oldTextInput = selectedBody.getTextInput();
            oldTextInput.text = oldTextInput.text.Replace("_", "");

            selectedBody.unSelect();
        }

        if (thisBodyActive)
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
        }
        else
        {
            if (retracting && backToIdle != null)
            {
                StopCoroutine(backToIdle);
            }

            retracting = false;
            scaleUp = ScaleTo(feedBack, feedBack.localScale, selectedScale, 0.3f);
            StartCoroutine(scaleUp);
            finishedScaling = false;
            expression.setSelectedExpr(expComp.getExpressionParent(), this);
        }

        thisBodyActive = !thisBodyActive;

        if (expComp == null)
            expComp = transform.parent.GetComponentInParent<ExpressionComponent>();

        //BUG: this code isn't running when you press t in a Vec Field expression
        if (variable)
        {
            title = transform.parent.parent.Find("VariableTitle").Find("Title").GetComponent<TMPro.TextMeshPro>().text;
            outputManager.HandleInput(transform.parent.name, title);
        }
        else
        {
            if (thisBodyActive)
            {
                outputManager.HandleInput(expComp.name, title);
            }
            else
            {
                calcInput.ChangeOutput(null);
            }
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
        thisBodyActive = false;
    }

    //BUG: null feedback when adding a new variable that's offscreen
    private void OnDisable()
    {
        if (feedBack && feedBack.localScale == selectedScale)
        {
            feedBack.localScale = idleScale;
            feedBack.gameObject.SetActive(false);

            ExpressionBody selectedBody = expression.getSelectedBody();
            TMPro.TextMeshPro oldTextInput = selectedBody.getTextInput();
            oldTextInput.text = oldTextInput.text.Replace("_", "");

            expression.setSelectedExpr(null, null);
            thisBodyActive = false;
            calcInput.ChangeOutput(null);
        }
    }

    void Update() { }
}
