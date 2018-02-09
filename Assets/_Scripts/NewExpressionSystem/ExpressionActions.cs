using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionActions : QuickButton
{
    Transform delete;
    Transform toggleHide;
    private Vector3 buttonActiveScale;

    private bool menuActive = false;
    private IEnumerator scaleButtonsUp, scaleButtonsDown;

    //TODO:
    // 1 - implement button functionality
    //      - deleting expressions      DONE
    //      - deleting variables        NEED HELP WITH UI 
    //      - toggle show and hide      DONE
    // 2 - make delete press open up a pop up window asking user to finalize the deletion
    //    - maybe make a generic popup window system?
    // 3 - create a delete mode where user can either delete entire expression or existing variables in that expression
    //      - need help coming up with design for this

    //BUGS:
    //

    private void Initialize()
    {
        delete = transform.parent.Find("Delete");
        toggleHide = transform.parent.Find("ToggleHide");

        delete.gameObject.SetActive(false);
        toggleHide.gameObject.SetActive(false);

        buttonActiveScale = new Vector3(1f, 1f, 1f);
    }

    protected override void Start()
    {
        base.Start();
        Initialize();
    }

    IEnumerator ScaleButtonsDown()
    {
        yield return StartCoroutine(ScaleTo(toggleHide, buttonActiveScale, Vector3.zero, 0.1f));
        toggleHide.gameObject.SetActive(false);

        yield return StartCoroutine(ScaleTo(delete, buttonActiveScale, Vector3.zero, 0.1f));
        delete.gameObject.SetActive(false);
    }

    IEnumerator ScaleButtonsUp()
    {
        delete.gameObject.SetActive(true);
        yield return StartCoroutine(ScaleTo(delete, Vector3.zero, buttonActiveScale, 0.1f));

        toggleHide.gameObject.SetActive(true);
        yield return StartCoroutine(ScaleTo(toggleHide, Vector3.zero, buttonActiveScale, 0.1f));
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
        if (menuActive)
        {
            if (scaleButtonsUp != null) StopCoroutine(scaleButtonsUp);

            scaleButtonsDown = ScaleButtonsDown();
            StartCoroutine(scaleButtonsDown);
        }
        else
        {
            if (scaleButtonsDown != null) StopCoroutine(scaleButtonsDown);

            scaleButtonsUp = ScaleButtonsUp();
            StartCoroutine(scaleButtonsUp);
        }

        menuActive = !menuActive;
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    private void OnDisable()
    {
        delete.localScale = Vector3.zero;
        toggleHide.localScale = Vector3.zero;
        menuActive = false;
    }

    void Update() { }
}
