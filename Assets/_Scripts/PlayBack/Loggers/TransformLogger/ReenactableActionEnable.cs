using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ReenactableActionEnable : ReenactableAction
{

    public override string key { get { return "enable"; } }
    public override void Reenact(LogInfo _info, GameObject subject, PlaybackLogEntry entry)
    {
        if (subject == null)
        {
             Debug.LogError("Could not reenact " + key + " becaused object with id " + entry.subjectKey + " does not exist");
            return;
        }

        subject.SetActive(true);

    }
}

