using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MatryxKeyboardButton : QuickButton {

    [SerializeField]
    Transform view;
    [SerializeField]
    RayCastButton menusButton;
    [SerializeField]
    RayCastButton matryxButton;

    protected override void ButtonEnterBehavior(GameObject other)
    {
        menusButton.PressButton(gameObject);
        matryxButton.PressButton(gameObject);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {

    }

}
