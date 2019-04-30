using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class QuickButton : MenuStateReceiver {

    protected virtual void Start()
    {
        VirtualButton virtualButton = GetComponent<VirtualButton>();
        if (virtualButton != null)
        {
            virtualButton.OnButtonPress += ButtonEnterBehavior;
            virtualButton.OnButtonUnpress += ButtonExitBehavior;
        }

        RayCastButton rcButton = GetComponent<RayCastButton>();
        if (rcButton != null)
        {
            rcButton.OnButtonPress += ButtonEnterBehavior;
            rcButton.OnButtonUnpress += ButtonExitBehavior;
        }

        TouchButton touchButton = GetComponent<TouchButton>();
        if (touchButton != null)
        {
            touchButton.OnButtonPress += ButtonEnterBehavior;
            touchButton.OnButtonUnpress += ButtonExitBehavior;
        }
    }

    protected abstract void ButtonEnterBehavior(GameObject other);

    protected abstract void ButtonExitBehavior(GameObject other);
}
