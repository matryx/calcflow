using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;
public class ReenactableActionDestroy : ReenactableAction
{

    public override string key { get { return "destroy"; } }
    public override void Reenact(LogInfo _info, GameObject subject, PlaybackLogEntry entry)    {

        if (subject != null)
        {
            UnityEngine.Object.Destroy(subject, 0);
        }
        else
        {
            Debug.Log(entry.timeStamp + " " + entry.subjectKey);
        }
    }
}
