using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class FuseButton : QuickButton
{

    public Transform CenterButton;

    private Vector3 idleScale = new Vector3(1f, 1f, 1f);
    private Vector3 selectedScale = new Vector3(1f, 1f, 1f);

    //true if the current locked position is fully expanded.
    private bool fullyActive = false;
    //true if it is either fully expanded or contracted.
    private bool finishedScaling = false;
    //true if it is partially expanded and not being clicked.
    private bool retract = false;

    private IEnumerator scaleMenuUp, scaleMenuDown;
    private IEnumerator backToSelected, backToIdle;


    void Update()
    {
        if (retract && (CenterButton.localScale == selectedScale ||
                           CenterButton.localScale == idleScale))
        {
            retract = false;
        }

        if (fullyActive && CenterButton.localScale == idleScale)
        {
            if (FuseCold != null)
                FuseCold.Invoke();
            fullyActive = false;
            finishedScaling = true;
        }
        else if (!fullyActive && CenterButton.localScale == selectedScale)
        {
            if (FuseHot != null)
                FuseHot.Invoke();
            fullyActive = true;
            finishedScaling = true;
        }
    }

    public delegate void FuseCallback();

    public event FuseCallback FuseHot;
    public event FuseCallback FuseCold;
    public void SetContractedScale(Vector3 smallScale)
    {
        idleScale = smallScale;
    }
    public void SetExpandedScale(Vector3 largeScale)
    {
        selectedScale = largeScale;
    }
    public void ForceHot()
    {
        CenterButton.localScale = selectedScale;
        if (FuseHot != null)
            FuseHot.Invoke();

    }
    public void ForceCold()
    {
        CenterButton.localScale = idleScale;
        if (FuseCold != null)
            FuseCold.Invoke();
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

    //BUG: when you poke and press menu button at the same time, secondary menu blinks
    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (fullyActive)
        {
            if (retract)
            {
                StopCoroutine(backToSelected);
                retract = false;
            }

            scaleMenuDown = ScaleTo(CenterButton, CenterButton.localScale, idleScale, 0.3f);
            StartCoroutine(scaleMenuDown);
        }
        else
        {
            if (retract)
            {
                StopCoroutine(backToIdle);
                retract = false;
            }

            scaleMenuUp = ScaleTo(CenterButton, CenterButton.localScale, selectedScale, 0.3f);
            StartCoroutine(scaleMenuUp);
        }
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        if (!finishedScaling)
        {
            if (fullyActive)
            {
                StopCoroutine(scaleMenuDown);
                backToSelected = ScaleTo(CenterButton, CenterButton.localScale, selectedScale, 0.5f);
                StartCoroutine(backToSelected);
            }
            else
            {
                StopCoroutine(scaleMenuUp);
                backToIdle = ScaleTo(CenterButton, CenterButton.localScale, idleScale, 0.5f);
                StartCoroutine(backToIdle);
            }

            retract = true;
        }

        finishedScaling = false;
    }
}
