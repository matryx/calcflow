using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using CalcFlowUI;

public class ButtonReenactableUnpress : ReenactableAction
{

    public override string key
    {
        get
        {
            return "buttonUnpress";
        }
    }

    public override void Reenact(LogInfo info, GameObject subject, PlaybackLogEntry entry)
    {
        Button button;
        GameObject buttonPresser;

        button = subject.GetComponent<Button>();
        buttonPresser = PlaybackLogEntry.GetObject(info.GetValue<string>("buttonPresser"));
        button.UnpressButton(buttonPresser);

    }

}
