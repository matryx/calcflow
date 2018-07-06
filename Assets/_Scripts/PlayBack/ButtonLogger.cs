using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using CalcFlowUI;

public class ButtonLogger// : PlayBackLogger
{

    // public override PlaybackLogAction2.ReenactAction[] GetReenactors()
    // {
    //     PlaybackLogAction2.ReenactAction[] reenactors = {
    //         ReenactPress,
    //         ReenactUnpress
    //         };
    // }

    // #region logging code
    // protected void Start()
    // {
    //     #region register to any and all buttons
    //     VirtualButton virtualButton = GetComponent<VirtualButton>();
    //     if (virtualButton != null)
    //     {
    //         virtualButton.OnButtonEnter += ButtonEnterBehavior;
    //         virtualButton.OnButtonExit += ButtonExitBehavior;
    //     }

    //     TouchRayButton rayTouchButton = GetComponent<TouchRayButton>();
    //     if (rayTouchButton != null)
    //     {
    //         rayTouchButton.OnButtonEnter += ButtonEnterBehavior;
    //         rayTouchButton.OnButtonExit += ButtonExitBehavior;
    //         return;
    //     }

    //     RayCastButton rcButton = GetComponent<RayCastButton>();
    //     if (rcButton != null)
    //     {
    //         rcButton.OnButtonEnter += ButtonEnterBehavior;
    //         rcButton.OnButtonExit += ButtonExitBehavior;
    //     }

    //     TouchButton touchButton = GetComponent<TouchButton>();
    //     if (touchButton != null)
    //     {
    //         touchButton.OnButtonEnter += ButtonEnterBehavior;
    //         touchButton.OnButtonExit += ButtonExitBehavior;
    //     }
    //     #endregion register to any and all buttons

    // }
    // protected void ButtonEnterBehavior(GameObject other)
    // {
    //     if (Recorder.Recording)
    //         Recorder.LogButtonPress(gameObject, other);
    // }

    // protected void ButtonExitBehavior(GameObject other)
    // {
    //     if (Recorder.Recording)
    //         Recorder.LogButtonUnpress(gameObject, other);
    // }
    // #endregion

    // #region reenact code

    // public static void ReenactPress(LogInfo _info, GameObject subject, PlaybackLogAction2 entry)
    // {
    //     Button button;
    //     GameObject buttonPresser;

    //     if (subject != null)
    //     {
    //         button = subject.GetComponent<Button>();
    //         buttonPresser = entry.getObject(_info.GetValue<int>("buttonPresser"));
    //         button.PressButton(buttonPresser);
    //     }
    //     else
    //     {
    //         Debug.Log(entry.timeStamp + " " + entry.subjectKey);
    //     }
    // }

    // public static void ReenactUnpress(LogInfo _info, GameObject subject, PlaybackLogAction2 entry)
    // {
    //     Button button;
    //     GameObject buttonPresser;
    //     if (subject != null)
    //     {
    //         button = subject.GetComponent<Button>();
    //         buttonPresser = entry.getObject(_info.GetValue<int>("buttonPresser"));
    //         button.UnpressButton(buttonPresser);
    //     }
    //     else
    //     {
    //         Debug.Log(entry.timeStamp + " " + entry.subjectKey);
    //     }
    // }
    // #endregion
}
