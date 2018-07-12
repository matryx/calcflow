using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using CalcFlowUI;
public class ButtonReenactablePress : ReenactableAction
{

    public override string key
    {
        get
        {
            return "buttonPress";
        }
    }
    public override void Reenact(LogInfo info, GameObject subject, PlaybackLogEntry entry)
    {
        Button button;
        GameObject buttonPresser;

        if (subject == null)
        {
            Debug.LogError("Could not reenact " + key + " becaused object with id " + entry.subjectKey + "does not exist");
            return;
        }

        button = subject.GetComponent<Button>();
        buttonPresser = PlaybackLogEntry.GetObject(info.GetValue<string>("buttonPresser"));
        button.PressButton(buttonPresser);

    }

}
