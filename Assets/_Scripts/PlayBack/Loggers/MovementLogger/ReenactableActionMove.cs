using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ReenactableActionMove : ReenactableAction
{

    public override string key { get { return "movement"; } }


    public override void Reenact(LogInfo info, GameObject subject, PlaybackLogEntry entry)
    {
        int parentKey;
        Vector3 position;
        Vector3 scale;
        Quaternion rotation;
        long duration;
        if (subject != null)
        {
            position = info.GetValue<Vector3>("position");
            scale = info.GetValue<Vector3>("scale");
            rotation = info.GetValue<Quaternion>("rotation");
            parentKey = info.GetValue<int>("parentKey");
            duration = info.GetValue<long>("duration");
            GameObject newParent;
            if (PlaybackLogEntry.TryGetObject(parentKey, out newParent))
            {
                subject.transform.SetParent((parentKey == 0) ? null : newParent.transform, false);
            }
            else
            {
                Debug.Log(entry.timeStamp + " " + subject.name + " could not reparent because parent " + parentKey + " does not exist.");
            }

            subject.LocalMoveTo(position, duration);
            subject.LocalRotateTo(rotation, duration);
            subject.LocalScaleTo(scale, duration);
        }
        else
        {
            Debug.Log(entry.timeStamp + " subject " + info.GetValue<int>("subjectKey") + " could not be moved because subject does not exist.");
        }

    }
}
