using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ButtonReenactableLogger : ReenactableLogger
{
    protected void Start()
    {
        #region register to any and all buttons
        VirtualButton virtualButton = GetComponent<VirtualButton>();
        if (virtualButton != null)
        {
            virtualButton.OnButtonEnter += ButtonEnterBehavior;
            virtualButton.OnButtonExit += ButtonExitBehavior;
        }

        TouchRayButton rayTouchButton = GetComponent<TouchRayButton>();
        if (rayTouchButton != null)
        {
            rayTouchButton.OnButtonEnter += ButtonEnterBehavior;
            rayTouchButton.OnButtonExit += ButtonExitBehavior;
            return;
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
        #endregion register to any and all buttons

    }
    protected void ButtonEnterBehavior(GameObject other)
    {
        if (Recorder.Recording)
        {
            long time = PlaybackClock.GetTime();
            Recorder.LogAction(PlaybackLogEntry.PlayBackActionFactory.CreateButtonPress(time, gameObject, other));
        }
    }

    protected void ButtonExitBehavior(GameObject other)
    {
        if (Recorder.Recording)
        {
            long time = PlaybackClock.GetTime();
            Recorder.LogAction(PlaybackLogEntry.PlayBackActionFactory.CreateButtonUnpress(time, gameObject, other));
        }
    }
}
