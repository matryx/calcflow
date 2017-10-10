using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AddExpression : QuickButton {
    public Transform constant;
    public Transform param;
    public Transform vecField;
    private bool addButtonsActive;

    protected override void Start()
    {
        base.Start();
        addButtonsActive = (constant.gameObject.activeSelf) ? true : false;
    }

    IEnumerator ScaleButtonsUp()
    {
        vecField.gameObject.SetActive(true);
        yield return StartCoroutine(ScaleTo(vecField, vecField.transform.localPosition, Vector3.one, 0.1f));

        param.gameObject.SetActive(true);
        yield return StartCoroutine(ScaleTo(param, param.transform.localPosition, Vector3.one, 0.1f));

        constant.gameObject.SetActive(true);
        yield return StartCoroutine(ScaleTo(constant, constant.transform.localPosition, Vector3.one, 0.1f));
    }

    IEnumerator ScaleButtonsDown()
    {
        yield return StartCoroutine(ScaleTo(constant, constant.transform.localPosition, Vector3.zero, 0.1f));
        constant.gameObject.SetActive(false);

        yield return StartCoroutine(ScaleTo(param, param.transform.localPosition, Vector3.zero, 0.1f));
        param.gameObject.SetActive(false);

        yield return StartCoroutine(ScaleTo(vecField, vecField.transform.localPosition, Vector3.zero, 0.1f));
        vecField.gameObject.SetActive(false);
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

    public void setAddActiveFalse()
    {
        StopAllCoroutines();
        StartCoroutine(ScaleButtonsDown());
        addButtonsActive = false;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        StopAllCoroutines();

        if (addButtonsActive)
        {
            StartCoroutine(ScaleButtonsDown());
        }
        else
        {
            StartCoroutine(ScaleButtonsUp());
        }

        addButtonsActive = !addButtonsActive;
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update () {
		
	}
}
