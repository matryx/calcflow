using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using CalcFlowUI;

public class MainMenu : QuickButton
{
    public Transform resetPos;
    public Transform resetScene;
    public Transform home;
    public Transform menus;
    public Transform matryx;
    public Transform outerRing, innerRing;

    private Vector3 idleScale, selectedScale;
    private Vector3 buttonScale;
    private Vector3 matryxScale;

    private bool menuActive = false;
    private bool finishedScaling = false;
    private bool retracting = false;

    private IEnumerator scaleMenuUp, scaleMenuDown;
    private IEnumerator backToSelected, backToIdle;

    protected override void Start()
    {
        base.Start();
        selectedScale = new Vector3(0.05f, 0.001f, 0.05f);
        idleScale = new Vector3(0.02f, 0.001f, 0.02f);
        buttonScale = new Vector3(0.003f, 0.05f, 0.07f);
        matryxScale = new Vector3(0.003f, 0.025f, 0.1475f);

        outerRing.SetParent(transform);
        innerRing.SetParent(transform);

        outerRing.SetParent(transform.parent);
        innerRing.SetParent(transform.parent);
        transform.localScale = idleScale;

        resetPos.gameObject.SetActive(false);
        resetScene.gameObject.SetActive(false);
        home.gameObject.SetActive(false);
        if(matryx != null)
        {
            matryx.gameObject.SetActive(false);
        }

        if (menus != null)
        {
            menus.gameObject.SetActive(false);
        }
    }

    void Update()
    {
        if (retracting && (transform.localScale == selectedScale ||
                           transform.localScale == idleScale))
        {
            retracting = false;
        }

        if (menuActive && transform.localScale == idleScale)
        {
            StartCoroutine(ScaleButtonsDown());
            menuActive = false;
            finishedScaling = true;
        }
        else if (!menuActive && transform.localScale == selectedScale)
        {
            StartCoroutine(ScaleButtonsUp());
            menuActive = true;
            finishedScaling = true;
        }
    }

    IEnumerator ScaleButtonsDown()
    {
        yield return StartCoroutine(ScaleTo(matryx, matryxScale, Vector3.zero, 0.1f));
        matryx.gameObject.SetActive(false);
        yield return StartCoroutine(ScaleTo(resetScene, buttonScale, Vector3.zero, 0.1f));
        resetScene.gameObject.SetActive(false);
        yield return StartCoroutine(ScaleTo(home, buttonScale, Vector3.zero, 0.1f));
        home.gameObject.SetActive(false);

        if (menus != null)
        {
            yield return StartCoroutine(ScaleTo(menus, buttonScale, Vector3.zero, 0.1f));
            menus.gameObject.SetActive(false);
        }

        yield return StartCoroutine(ScaleTo(resetPos, buttonScale, Vector3.zero, 0.1f));
        resetPos.gameObject.SetActive(false);
    }

    IEnumerator ScaleButtonsUp()
    {
        resetPos.gameObject.SetActive(true);
        yield return StartCoroutine(ScaleTo(resetPos, Vector3.zero, buttonScale, 0.1f));

        if (menus != null)
        {
            menus.gameObject.SetActive(true);
            yield return StartCoroutine(ScaleTo(menus, Vector3.zero, buttonScale, 0.1f));
        }

        home.gameObject.SetActive(true);
        yield return StartCoroutine(ScaleTo(home, Vector3.zero, buttonScale, 0.1f));
        resetScene.gameObject.SetActive(true);
        yield return StartCoroutine(ScaleTo(resetScene, Vector3.zero, buttonScale, 0.1f));
        matryx.gameObject.SetActive(true);
        yield return StartCoroutine(ScaleTo(matryx, Vector3.zero, matryxScale, 0.1f));
        
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
        if (menuActive)
        {
            if (retracting)
            {
                StopCoroutine(backToSelected);
                retracting = false;
            }

            scaleMenuDown = ScaleTo(transform, transform.localScale, idleScale, 0.3f);
            StartCoroutine(scaleMenuDown);
        }
        else
        {
            if (retracting)
            {
                StopCoroutine(backToIdle);
                retracting = false;
            }

            scaleMenuUp = ScaleTo(transform, transform.localScale, selectedScale, 0.3f);
            StartCoroutine(scaleMenuUp);
        }
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        if (!finishedScaling)
        {
            if (menuActive)
            {
                StopCoroutine(scaleMenuDown);
                backToSelected = ScaleTo(transform, transform.localScale, selectedScale, 0.5f);
                StartCoroutine(backToSelected);
            }
            else
            {
                StopCoroutine(scaleMenuUp);
                backToIdle = ScaleTo(transform, transform.localScale, idleScale, 0.5f);
                StartCoroutine(backToIdle);
            }

            retracting = true;
        }

        finishedScaling = false;
    }
}
