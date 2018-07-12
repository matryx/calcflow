using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;
public class ReenactableActionDisable : ReenactableAction
{
    public override string key { get { return "disable"; } }
    public override void Reenact(LogInfo _info, GameObject subject, PlaybackLogEntry entry)
    {

        subject.SetActive(false);
    }
}
