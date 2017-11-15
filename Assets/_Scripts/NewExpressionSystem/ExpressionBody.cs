using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionBody : QuickButton {
    Expressions expression;
    ExpressionComponent expComp;
    Transform feedBack;
    TMPro.TextMeshPro textInput;
    TMPro.TextMeshPro title;
    string varTitle;
    OutputManager outputManager;
    
    private bool menuActive = false;
    private bool finishedScaling = false;
    private bool retracting = false;

    private Vector3 idleScale, selectedScale;

    private IEnumerator scaleMenuUp, scaleMenuDown;
    private IEnumerator backToSelected, backToIdle;

    protected override void Start()
    {
        base.Start();
        expression = GameObject.Find("Expressions").GetComponent<Expressions>();
        expComp = transform.GetComponentInParent<ExpressionComponent>();
        feedBack = transform.parent.Find("Feedback");
        textInput = transform.parent.Find("Text_Input").GetComponent<TMPro.TextMeshPro>();
        title = transform.parent.Find("Title").GetComponent<TMPro.TextMeshPro>();
        outputManager = expression.GetComponent<OutputManager>();

        selectedScale = new Vector3(4.56999f, 0.04f, 0.002f);
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

    public TMPro.TextMeshPro getTitle()
    {
        return title;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (menuActive)
        {
            if (retracting)
            {
                StopCoroutine(backToSelected);
                retracting = false;
            }

            scaleMenuDown = ScaleTo(feedBack, feedBack.localScale, idleScale, 0.3f);
            StartCoroutine(scaleMenuDown);
        }
        else
        {
            if (retracting)
            {
                StopCoroutine(backToIdle);
                retracting = false;
            }

            scaleMenuUp = ScaleTo(feedBack, feedBack.localScale, selectedScale, 0.3f);
            StartCoroutine(scaleMenuUp);
        }

        if (expComp == null) {
            expComp = transform.parent.GetComponentInParent<ExpressionComponent>();
        }

        expression.setSelectedExpr(expComp.getExpressionParent(), this);
        if (expComp.getExpressionParent().GetComponent<ParametricExpression>()) 
            varTitle = expComp.getExpressionParent().GetComponent<ParametricExpression>().getVarTitle(transform.parent.parent); 
        outputManager.HandleInput(expComp.name, varTitle);
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
        if (end == idleScale) obj.gameObject.SetActive(false);
    }

    public void unSelect()
    {
        if (!expComp.getPanel().gameObject.activeSelf || !transform.parent.gameObject.activeSelf) {
            feedBack.localScale = idleScale;
            return;
        }

        if (!finishedScaling)
        {
            if (menuActive)
            {
                StopCoroutine(scaleMenuDown);
                backToSelected = ScaleTo(feedBack, feedBack.localScale, selectedScale, 0.5f);
                StartCoroutine(backToSelected);
            }
            else
            {
                StopCoroutine(scaleMenuUp);
                backToIdle = ScaleTo(feedBack, feedBack.localScale, idleScale, 0.5f);
                StartCoroutine(backToIdle);
            }

            retracting = true;
        }

        finishedScaling = false;
    }

    void Update() { }
}
