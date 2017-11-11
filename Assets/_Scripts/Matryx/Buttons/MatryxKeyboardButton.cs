using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MatryxKeyboardButton : QuickButton {

    [SerializeField]
    RayCastButton menusButton;
    [SerializeField]
    RayCastButton matryxButton;


    protected override void ButtonEnterBehavior(GameObject other)
    {
        StartCoroutine(timedPressButtons());
    }

    protected override void ButtonExitBehavior(GameObject other)
    {

    }

    protected IEnumerator timedPressButtons()
    {
        menusButton.PressButton(gameObject);
        yield return new WaitForSeconds(0.1f);
        matryxButton.PressButton(gameObject);
    }
}
