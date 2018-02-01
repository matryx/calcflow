using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ButtonLogger : QuickButton
{
    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (Recorder.Recording)
            Recorder.LogButtonPress(gameObject, other);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        if (Recorder.Recording)
            Recorder.LogButtonUnpress(gameObject, other);
    }
}
