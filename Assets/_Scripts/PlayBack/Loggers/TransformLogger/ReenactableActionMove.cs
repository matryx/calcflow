using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ReenactableActionMove : ReenactableAction
{

    public override string key { get { return "movement"; } }


    public override void Reenact(LogInfo info, GameObject subject, PlaybackLogEntry entry)
    {
        string parentKey;
        Vector3 position;
        Vector3 scale;
        Quaternion rotation;
        long duration;

        if (subject == null)
        {
            Debug.LogError(entry.timeStamp + " " + "Could not reenact " + key + " on object " + entry.name + " because object with id " + entry.subjectKey + "does not exist");
            return;
        }

        position = info.GetValue<Vector3>("position");
        scale = info.GetValue<Vector3>("scale");
        rotation = info.GetValue<Quaternion>("rotation");
        parentKey = info.GetValue<string>("parentKey");
        duration = info.GetValue<long>("duration");
        GameObject newParent;
        if (PlaybackLogEntry.TryGetObject(parentKey, out newParent))
        {
            subject.transform.SetParent((parentKey == "") ? null : newParent.transform, false);
        }
        else
        {
            Debug.Log(entry.timeStamp + " " + subject.name + " could not reparent because parent " + parentKey + " does not exist.");
        }

        subject.LocalMoveTo(position, ((float)duration) / 1000f);
        subject.LocalRotateTo(rotation, ((float)duration) / 1000f);
        subject.LocalScaleTo(scale, ((float)duration) / 1000f);
    }
}
