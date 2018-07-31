using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]
public class TabButton : QuickButton
{
    public Transform thisMenu;
    public Transform[] otherMenus;
    public Transform collapsedButton;

    Vector3 uncollapsedPos, uncollapsedScale, buttonPos;

    protected override void Start()
    {
        base.Start();

        uncollapsedPos = new Vector3(1.876f, -3.52f, 0.007f);
        uncollapsedScale = new Vector3(1f, 1f, 1f);
        buttonPos = new Vector3(1.882f, -6.718f, -0.025f);
    }

    void Update() { }

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

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (thisMenu.localScale != uncollapsedScale &&
            thisMenu.localPosition != uncollapsedPos)
        {
            thisMenu.gameObject.SetActive(true);

            StartCoroutine(ScaleTo(thisMenu, thisMenu.localScale, uncollapsedScale, 0.5f));
            StartCoroutine(MoveTo(thisMenu, thisMenu.localPosition, uncollapsedPos, 0.5f));

            foreach (Transform t in otherMenus)
            {
                StartCoroutine(ScaleTo(t, t.localScale, uncollapsedScale, 0.5f));
                StartCoroutine(MoveTo(t, t.localPosition, uncollapsedPos, 0.5f));
                StartCoroutine(MoveTo(collapsedButton, collapsedButton.localPosition, buttonPos, 0.5f));
            }

            collapsedButton.localEulerAngles += new Vector3(0, 0, 180f);
        }
    }

    protected override void ButtonExitBehavior(GameObject other) { }
}
