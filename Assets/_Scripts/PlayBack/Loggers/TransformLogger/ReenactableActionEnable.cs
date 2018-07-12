using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ReenactableActionEnable : ReenactableAction
{

    public override string key { get { return "enable"; } }
    public override void Reenact(LogInfo _info, GameObject subject, PlaybackLogEntry entry)
    {
        subject.SetActive(true);

    }
}
