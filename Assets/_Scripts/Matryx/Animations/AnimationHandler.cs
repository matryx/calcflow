using Nanome.Core;
using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AnimationHandler : MonoBehaviour {

    bool retracting;
    private IEnumerator scaleMenuDown, scaleMenuUp;
    private Vector3 activeScale, inactiveScale;

    private float speed = 0.2f;
    private bool opening, closing;
    private bool open = false;
    private bool closed = true;

    public void Awake()
    {
        activeScale = transform.localScale;
        inactiveScale = Vector3.zero;
        transform.localScale = Vector3.zero;
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

    IEnumerator ShrinkMenu(Async.EventDelegate onFinish = null)
    {
        closing = true;
        yield return StartCoroutine(ScaleTo(transform, transform.localScale, Vector3.zero, speed));
        gameObject.SetActive(false);
        closing = false;
        closed = true;
        onFinish?.Invoke(this);
    }
    
    IEnumerator GrowMenu(Async.EventDelegate onFinish = null)
    {
        opening = true;
        yield return StartCoroutine(ScaleTo(transform, transform.localScale, activeScale, speed));
        opening = false;
        open = true;
        onFinish?.Invoke(this);
    }

    public void CloseMenu(Async.EventDelegate onFinish = null)
    {
        if(open)
        {
            scaleMenuDown = ShrinkMenu(onFinish);
            StartCoroutine(scaleMenuDown);
        }
        else if(opening)
        {
            StopCoroutine(scaleMenuUp);
            scaleMenuDown = ShrinkMenu(onFinish);
            StartCoroutine(scaleMenuDown);
        }
    }

    public void OpenMenu(Async.EventDelegate onFinish = null)
    {
        if (closed)
        {
            gameObject.SetActive(true);
            scaleMenuUp = GrowMenu(onFinish);
            StartCoroutine(scaleMenuUp);
        }
        else if (closing)
        {
            StopCoroutine(scaleMenuDown);
            scaleMenuUp = GrowMenu(onFinish);
            StartCoroutine(scaleMenuUp);
        }
    }
}
