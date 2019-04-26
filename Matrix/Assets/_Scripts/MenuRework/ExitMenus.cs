using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExitMenus : QuickButton
{
    public Transform menu;
    bool retracting;

    private IEnumerator scaleCircleDown, backToActive;

    private Vector3 activeScale, inactiveScale;

    // Use this for initialization
    protected override void Start()
    {
        base.Start();

        activeScale = new Vector3(0.5f, 0.01f, 0.5f);
        inactiveScale = new Vector3(0.2f, 0.01f, 0.2f);
    }

    // Update is called once per frame
    void Update()
    {
        if (retracting && (transform.localScale == activeScale || 
                           transform.localScale == inactiveScale))
        {
            retracting = false;
        }

        if (transform.localScale == inactiveScale)
        {
            transform.localScale = activeScale;
            StartCoroutine(menu.GetComponent<ExpandContract>().Contract());
            //StartCoroutine(DeActivate());
        }
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

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (retracting)
        {
            StopCoroutine(backToActive);
            retracting = false;
        }
        scaleCircleDown = ScaleTo(transform, transform.localScale, inactiveScale, 0.3f);
        StartCoroutine(scaleCircleDown);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        StopCoroutine(scaleCircleDown);
        backToActive = ScaleTo(transform, transform.localScale, activeScale, 0.5f);
        StartCoroutine(backToActive);

        retracting = true;
    }
}
