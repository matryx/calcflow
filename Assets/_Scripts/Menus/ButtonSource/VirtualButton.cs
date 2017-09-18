using CalcFlowUI;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VirtualButton : Button
{
#if UNITY_EDITOR
    public bool press = false;
    bool pressed = false;
    bool Pressed
    {
        get
        {
            return pressed;
        }
        set
        {
            if (!pressed && value)
            {
                pressed = value;
                PressButton(this.gameObject);
            } else if (pressed && !value)
            {
                pressed = value;
                UnpressButton(this.gameObject);
            }
        }
    }
    public void Update()
    {
        Pressed = press;
    }
#endif
}
