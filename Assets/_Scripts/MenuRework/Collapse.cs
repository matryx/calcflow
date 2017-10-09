using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Collapse : QuickButton {
    public Transform[] menus;

    private bool animating;
    private IEnumerator routine;

    Transform currMenu;
    Vector3 collapsedPos, uncollapsedPos;
    Vector3 collapsedScale, uncollapsedScale;
    Vector3 targetScale, targetPos, buttonPos;
    Vector3 buttonCollPos, buttonUncollPos;

    protected override void Start () {
        base.Start();

        collapsedPos = new Vector3(1.876f, -0.54f, 0f);
        uncollapsedPos = new Vector3(1.876f, -3.52f, 0.006999969f);

        collapsedScale = new Vector3(1f, 0f, 1f);
        uncollapsedScale = new Vector3(1f, 1f, 1f);

        buttonCollPos = new Vector3(1.882f, -0.75f, 0);
        buttonUncollPos = new Vector3(1.882f, -6.718f, -0.025f);
    }
	
	void Update ()
    {
        foreach(Transform t in menus)
        {
            if (t.gameObject.activeSelf) currMenu = t;
        }
	}

    IEnumerator MoveTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            obj.localPosition = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        obj.localPosition = end;
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

    IEnumerator Animate()
    {
        foreach(Transform t in menus)
        {
            StartCoroutine(ScaleTo(t, t.localScale, targetScale, 0.5f));
            StartCoroutine(MoveTo(t, t.localPosition, targetPos, 0.5f));
            StartCoroutine(MoveTo(transform, transform.localPosition, buttonPos, 0.5f));
        }

        yield return new WaitUntil(doneAnimating);
    }

    bool doneAnimating()
    {
        return (currMenu.localScale == targetScale && currMenu.localPosition == targetPos);
    }

    IEnumerator CollapseMenu()
    {
        targetScale = collapsedScale;
        targetPos = collapsedPos;
        buttonPos = buttonCollPos;
        yield return StartCoroutine(Animate());
        animating = false;
        transform.localEulerAngles -= new Vector3(0, 0, 180f);
        currMenu.gameObject.SetActive(false);
    }

    IEnumerator UncollapseMenu()
    {
        currMenu.gameObject.SetActive(true);
        targetScale = uncollapsedScale;
        targetPos = uncollapsedPos;
        buttonPos = buttonUncollPos;
        yield return StartCoroutine(Animate());
        transform.localEulerAngles += new Vector3(0, 0, 180f);
        animating = false;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (!animating)
        {
            animating = true;
            routine = (currMenu.gameObject.activeSelf) ?
                       CollapseMenu() : UncollapseMenu();
            StartCoroutine(routine);
        }
    }

    protected override void ButtonExitBehavior(GameObject other) { }
}
