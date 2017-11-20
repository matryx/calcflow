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

    private bool menuActive = false;
    private bool finishedScaling = false;
    private bool retracting = false;
    private bool variable = false;

    private Vector3 idleScale, selectedScale;

    private IEnumerator scaleMenuUp, scaleMenuDown;
    private IEnumerator backToSelected, backToIdle;

    protected override void Start()
    {
        base.Start();
        expression = GameObject.Find("Expressions").GetComponent<Expressions>();

        feedBack = transform.parent.Find("Feedback");
        if (!feedBack)
        {
            feedBack = transform.parent.parent.Find("Feedback");
            variable = true;
        }

        expComp = (variable) ? transform.parent.GetComponentInParent<ExpressionComponent>() :
                               transform.GetComponentInParent<ExpressionComponent>();

        if (transform.parent.Find("Text_Input"))
            textInput = transform.parent.Find("Text_Input").GetComponent<TMPro.TextMeshPro>();

        //BUG: null error when creating vector field expression
        title = transform.parent.Find("Title").GetComponent<TMPro.TextMeshPro>().text.Substring(0, 1);
        outputManager = expression.GetComponent<OutputManager>();

        selectedScale = (variable) ? new Vector3(2.16f, 0.04f, 0.002f) :
                                     new Vector3(4.56999f, 0.04f, 0.002f);
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

    public string getTitle()
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
            finishedScaling = false;
            expression.setSelectedExpr(null, null);
            print("CALL 1");
        }
        else
        {
            if (retracting && backToIdle != null)
            {
                StopCoroutine(backToIdle);
            }

            retracting = false;
            scaleMenuUp = ScaleTo(feedBack, feedBack.localScale, selectedScale, 0.3f);
            StartCoroutine(scaleMenuUp);
            finishedScaling = false;
            expression.setSelectedExpr(expComp.getExpressionParent(), this);
            print("CALL 2");
        }

        if (expComp == null)
        {
            expComp = transform.parent.GetComponentInParent<ExpressionComponent>();
        }

        if (variable)
        {
            outputManager.HandleInput(transform.parent.name, title);
        }
        else
        {
            outputManager.HandleInput(expComp.name, title);
        }

        menuActive = !menuActive;
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

    //BUG: not doing this right, scalemenudown returns null
    public void unSelect()
    {
        //if (!expComp.getPanel().gameObject.activeSelf || !transform.parent.gameObject.activeSelf) {
        //    feedBack.localScale = idleScale;
        //    //expression.setSelectedExpr(null, null);
        //    return;
        //}
        print("CALL 3");

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

            menuActive = !menuActive;
            retracting = true;
        }

        expression.setSelectedExpr(null, null);
        finishedScaling = true;
    }

    private void OnDisable()
    {
        if (feedBack.localScale == selectedScale)
        {
            feedBack.localScale = idleScale;
            feedBack.gameObject.SetActive(false);
            expression.setSelectedExpr(null, null);
            print("CALL 4");
        }
    }

    void Update()
    {
        //if (!expComp.getPanel().gameObject.activeSelf) {
        //    print("switched panels");
        //    feedBack.localScale = idleScale;
        //}
    }
}
