using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class QuickButton : MenuStateReceiver {

    protected virtual void Start()
    {
        VirtualButton virtualButton = GetComponent<VirtualButton>();
        if (virtualButton != null)
        {
            virtualButton.OnButtonEnter += ButtonEnterBehavior;
            virtualButton.OnButtonExit += ButtonExitBehavior;
        }

        RayCastButton rcButton = GetComponent<RayCastButton>();
        if (rcButton != null)
        {
            rcButton.OnButtonEnter += ButtonEnterBehavior;
            rcButton.OnButtonExit += ButtonExitBehavior;
        }

        TouchButton touchButton = GetComponent<TouchButton>();
        if (touchButton != null)
        {
            touchButton.OnButtonEnter += ButtonEnterBehavior;
            touchButton.OnButtonExit += ButtonExitBehavior;
        }
    }

    protected abstract void ButtonEnterBehavior(GameObject other);

    protected abstract void ButtonExitBehavior(GameObject other);
}
