using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionActions : QuickButton
{
    List<Transform> buttons;
    Transform delete;
    Transform toggleHide;
    Transform select;
    private Vector3 buttonActiveScale;

    private bool menuActive = false;
    private IEnumerator scaleButtonsUp, scaleButtonsDown;

    private void Initialize()
    {
        buttons = new List<Transform>();

        Transform check = transform.parent.Find("Delete");
        if (check != null) buttons.Add(check);

        check = transform.parent.Find("ToggleHide");
        if (check != null) buttons.Add(check);

        check = transform.parent.Find("Select");
        if (check != null) buttons.Add(check);

        foreach (Transform b in buttons)
        {
            b.gameObject.SetActive(false);
        }

        buttonActiveScale = new Vector3(1f, 1f, 1f);
    }

    protected override void Start()
    {
        base.Start();
        Initialize();
    }

    public void disableButtons()
    {
        if (scaleButtonsUp != null) StopCoroutine(scaleButtonsUp);

        scaleButtonsDown = ScaleButtonsDown();
        StartCoroutine(scaleButtonsDown);

        menuActive = false;
    }

    IEnumerator ScaleButtonsDown()
    {
        foreach (Transform b in buttons)
        {
            yield return StartCoroutine(ScaleTo(b, buttonActiveScale, Vector3.zero, 0.1f));
            b.gameObject.SetActive(false);
        }
    }

    IEnumerator ScaleButtonsUp()
    {
        buttons.Reverse();

        foreach (Transform b in buttons)
        {
            b.gameObject.SetActive(true);
            yield return StartCoroutine(ScaleTo(b, Vector3.zero, buttonActiveScale, 0.1f));
        }

        buttons.Reverse();
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
        foreach(Transform b in buttons)
        {
            if (b.gameObject.activeSelf)
            {
                b.localScale = Vector3.zero;
                b.gameObject.SetActive(false);
            }
        }

        menuActive = false;
    }

    void Update() { }
}
