using CalcFlowUI;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
[RequireComponent(typeof(RayCastButton))]
[RequireComponent(typeof(TouchButton))]
public class TouchRayButton : Button {
    RayCastButton rcButton;
    TouchButton touchButton;

    // Use this for initialization
    void Start () {
         rcButton = GetComponent<RayCastButton>();
         touchButton = GetComponent<TouchButton>();

        if(rcButton == null)
        {
            rcButton = gameObject.AddComponent<RayCastButton>();
        }
        if (touchButton == null)
        {
            touchButton = gameObject.AddComponent<TouchButton>();
        }

        rcButton.OnButtonEnter += PressButton;
        rcButton.OnButtonExit += UnpressButton;
        touchButton.OnButtonEnter += PressButton;
        touchButton.OnButtonExit += UnpressButton;
    }


    private void Update()
    {
        if (GetComponent<RayCastButton>() == null)
            gameObject.AddComponent<RayCastButton>();
        if (GetComponent<TouchButton>() == null)
            gameObject.AddComponent<TouchButton>();

    }
}
