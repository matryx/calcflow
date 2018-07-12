using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;

public class LoadingScreen : MonoBehaviour
{
    private float limit = 0;
    //out of 1
    private float percent = 0;
    TextMeshPro loadingText;
    Transform loadingBar;
    float barStartingWidth = 0;
    bool loading;
    public void StartLoading()
    {
        loading = true;
        loadingText = GetComponentInChildren<TextMeshPro>();
        loadingBar = transform.Find("LoadingBar").transform;
        barStartingWidth = loadingBar.localScale.x;
        StartCoroutine(UpdateStatus());
    }

    public void StopLoading()
    {
        loading = false;
    }

    private void UpdateText(int numDots)
    {
        string newText = "Loading";
        for (int i = 0; i < numDots; i++)
        {
            newText = newText + ".";
        }
        loadingText.text = newText;
    }

    private void UpdateBar()
    {
        float newscale = barStartingWidth * percent;
        loadingBar.localScale = new Vector3(barStartingWidth * percent, loadingBar.localScale.y, loadingBar.localScale.z);
        loadingBar.localPosition = new Vector3(-2 + percent * 2, loadingBar.localPosition.y, loadingBar.localPosition.z);
    }

    IEnumerator UpdateStatus()
    {
        int numDots = 0;
        while (loading)
        {
            numDots = (numDots + 1) % 4;
            UpdateText(numDots);

            yield return new WaitForSeconds(.25f);
        }
    }

    //determines what number will represent the full bar.
    public void SetBarLimit(float limit)
    {
        this.limit = limit;
    }
    //sets current bar progress
    public void SetRemaining(float status)
    {
        percent = 1 - status / limit;
        UpdateBar();
    }

    //returns percent (between 0 and 1)
    public float GetPercentComplete()
    {
        return percent;
    }
}