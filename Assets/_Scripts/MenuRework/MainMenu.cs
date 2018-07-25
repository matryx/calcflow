using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using CalcFlowUI;

public class MainMenu : MonoBehaviour
{
    private Vector3 buttonScale;
    private Vector3 matryxScale;

    private Transform resetPos, resetScene, home, menus, matryx;
    private void FindButtons()
    {
        home = this.transform.parent.Find("HomeButton");
        menus = this.transform.parent.Find("MenuButton");
        resetPos = this.transform.parent.Find("ResetPositionButton");
        resetScene = this.transform.parent.Find("ResetSceneButton");
        matryx = this.transform.parent.Find("MatryxButton");
    }

    private void DeactivateButtons()
    {
        resetPos.gameObject.SetActive(false);
        resetScene.gameObject.SetActive(false);
        home.gameObject.SetActive(false);
        if (matryx != null)
        {
            matryx.gameObject.SetActive(false);
        }

        if (menus != null)
        {
            menus.gameObject.SetActive(false);
        }
    }

    FuseButton fuseButton;

    protected void Start()
    {
        Vector3 idleScale, selectedScale;

        FindButtons();
        selectedScale = new Vector3(0.02f, 0.0004f, 0.02f);
        fuseButton.SetExpandedScale(selectedScale);

        idleScale = new Vector3(0.0075f, 0.0004f, 0.0075f);
        fuseButton.SetContractedScale(idleScale);

        buttonScale = new Vector3(0.003f, 0.05f, 0.07f);
        matryxScale = new Vector3(0.003f, 0.025f, 0.1475f);

        fuseButton = GetComponent<FuseButton>();
        fuseButton.FuseHot += HideButtons;
        fuseButton.FuseCold += RevealButtons;

        DeactivateButtons();
    }
    void HideButtons()
    {
        StartCoroutine(HideButtonsCoroutine());
    }

    void RevealButtons()
    {
        StartCoroutine(RevealButtonsCoroutine());
    }
    IEnumerator HideButtonsCoroutine()
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
    IEnumerator RevealButtonsCoroutine()
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



}
