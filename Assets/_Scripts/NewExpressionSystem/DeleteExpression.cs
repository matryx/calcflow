using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DeleteExpression : QuickButton
{

    Expressions expressions;
    private FlexMenu keyboard;
    JoyStickAggregator joyStickAggregator;

    Transform popup;
    float distance = -0.04f;
    Transform deleteButton;

    protected override void Start()
    {
        base.Start();
        expressions = Expressions._instance;
        popup = transform.parent.parent.parent.Find("DeleteConfirmation");
        deleteButton = transform.parent;
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
        if (!popup.gameObject.activeSelf)
        {
            popup.gameObject.SetActive(true);
            popup.position = deleteButton.position + new Vector3(0, -0.8f, -0.1f);
            StartCoroutine(ScaleTo(popup, Vector3.zero, Vector3.one, 0.1f));
        }
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
