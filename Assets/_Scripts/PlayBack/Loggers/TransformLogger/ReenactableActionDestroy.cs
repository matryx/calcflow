using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;
public class ReenactableActionDestroy : ReenactableAction
{

    public override string key { get { return "destroy"; } }
    public override void Reenact(LogInfo _info, GameObject subject, PlaybackLogEntry entry)
    {
        UnityEngine.Object.Destroy(subject, 0);
    }
}
