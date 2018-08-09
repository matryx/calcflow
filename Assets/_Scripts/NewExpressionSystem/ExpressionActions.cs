using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionActions : QuickButton
{
    List<Transform> buttons;
    Transform delete;
    Transform toggleHide;
    Transform select;

    IEnumerator scaleButtonsUp, scaleButtonsDown;

    bool menuActive = false;
    Vector3 buttonActiveScale;

    void Initialize()
    {
        buttons = new List<Transform>();

        LookForButton("Delete");
        LookForButton("ToggleHide");

        foreach (Transform b in buttons)
        {
            b.gameObject.SetActive(false);
        }

        buttonActiveScale = new Vector3(1f, 1f, 1f);
    }

    void LookForButton(string name)
    {
        Transform check = transform.parent.Find(name);
        if (check != null) buttons.Add(check);
    }

    protected override void Start()
    {
        base.Start();
        Initialize();
    }

    public void DisableButtons()
    {
        if (!menuActive) return;

        if (scaleButtonsUp != null) StopCoroutine(scaleButtonsUp);

        scaleButtonsDown = ScaleButtonsDown();
        StartCoroutine(scaleButtonsDown);

        menuActive = false;
    }

    IEnumerator ScaleButtonsDown()
    {
        buttons.Reverse();

        foreach (Transform b in buttons)
        {
            yield return StartCoroutine(ScaleTo(b, buttonActiveScale, Vector3.zero, 0.1f));
            b.gameObject.SetActive(false);
        }

        buttons.Reverse();
    }

    IEnumerator ScaleButtonsUp()
    {
        foreach (Transform b in buttons)
        {
            b.gameObject.SetActive(true);
            yield return StartCoroutine(ScaleTo(b, Vector3.zero, buttonActiveScale, 0.1f));
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

    void OnDisable()
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
