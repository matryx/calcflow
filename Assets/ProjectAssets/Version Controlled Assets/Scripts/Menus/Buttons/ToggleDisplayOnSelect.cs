using UnityEngine;
using CalcFlowUI;
using System;

public class ToggleDisplayOnSelect : QuickButton
{
    public GameObject[] elements;

    private bool isDisplayed;

	// Use this for initialization
	protected override void Start ()
    {
        base.Start();
        isDisplayed = false;
        HideElements();
    }

    public void ShowElements()
    {
        for (int i = 0; i < elements.Length; i++)
        {
            elements[i].SetActive(true);
        }
        isDisplayed = true;
    }


    public void HideElements()
    {
        for (int i = 0; i < elements.Length; i++)
        {
            elements[i].SetActive(false);
        }
        isDisplayed = false;
    }


    protected override void ButtonEnterBehavior(GameObject other)
    {
        isDisplayed = !isDisplayed;

        if (isDisplayed == true)
        {
            ShowElements();
        }
        else
        {
            HideElements();
        }
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
    }
}
