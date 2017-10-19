using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ButtonLogger : QuickButton
{
    protected override void ButtonEnterBehavior(GameObject other)
    {
        PlaybackLog.LogButtonPress(gameObject, other);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        PlaybackLog.LogButtonUnpress(gameObject, other);
    }
}
