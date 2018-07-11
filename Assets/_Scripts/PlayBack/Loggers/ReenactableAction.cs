using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class ReenactableAction
{
    public abstract void Reenact(LogInfo info, GameObject subject, PlaybackLogEntry entry);
    public abstract string key { get; }

}
