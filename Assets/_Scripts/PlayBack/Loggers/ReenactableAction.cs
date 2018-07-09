using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class ReenactableAction
{
    public abstract void Reenact(LogInfo info, GameObject subject, PlaybackLogAction2 entry);
    public abstract string key { get; }

}
