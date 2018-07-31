using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using CalcFlowUI;

public class MatryxKeyboardButton : QuickButton
{

    Button menusButton;
    Button matryxButton;


    protected override void Start()
    {
        base.Start();
        Transform menu = transform.parent.Find("MenuButton");
        SecondaryMenu secondaryMenu = SecondaryMenu.GetInstance();
        if (secondaryMenu != null && menu != null)
        {
            menusButton = menu.GetComponent<Button>();
            matryxButton = secondaryMenu.transform.Find("Panel/Matryx").GetComponent<Button>();
        }
        else
        {
            Destroy(gameObject);
        }
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        StartCoroutine(timedPressButtons());
    }

    protected override void ButtonExitBehavior(GameObject other)
    {

    }

    protected IEnumerator timedPressButtons()
    {

        if (!matryxButton.gameObject.activeInHierarchy)
        {
            menusButton.PressButton(gameObject);
            menusButton.UnpressButton(gameObject);

            yield return null;
            yield return null;

            yield return null;
            yield return null;

            matryxButton.PressButton(gameObject);
            matryxButton.UnpressButton(gameObject);
        }
    }
}
