using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ReenactableActionEnable : ReenactableAction
{

    public override string key { get { return "enable"; } }
    public override void Reenact(LogInfo _info, GameObject subject, PlaybackLogEntry entry)
    {
        int parentKey;
        Vector3 position;
        Vector3 scale;
        Quaternion rotation;
        long duration;

        // if (objectMap.ContainsKey(subjectKey))
        // {
        //     subject = objectMap[subjectKey];
        //     subject.SetActive(true);
        // }
        // else
        // {
        //     Debug.Log(timeStamp + " " + subjectKey);
        // }
    }
}
