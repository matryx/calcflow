using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionBody : QuickButton {
    Expressions expression;
    Transform feedBack;

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
        feedBack = transform.parent.Find("Feedback");

        selectedScale = new Vector3(4.56999f, 0.04f, 0.002f);
        idleScale = new Vector3(0f, 0.04f, 0.002f);
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

        ExpressionComponent expComp = transform.GetComponentInParent<ExpressionComponent>();
        if (expComp == null) {
            expComp = transform.parent.GetComponentInParent<ExpressionComponent>();
        }

        expression.setSelectedExpr(expComp.getExpressionParent());
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
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

    IEnumerator ScaleTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            obj.localScale = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        obj.localScale = end;
    }

    void Update () { }
}
