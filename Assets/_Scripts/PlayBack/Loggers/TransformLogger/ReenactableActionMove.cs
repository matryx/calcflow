using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ReenactableActionMove : ReenactableAction
{

    public override string key { get { return "movement"; } }

    public override void Reenact(LogInfo info, GameObject subject, PlaybackLogEntry entry)
    {

        if (subject == null)
        {
            Debug.LogError(entry.timeStamp + " " + "Could not reenact " + key + " on object " + entry.name + " because object with id " + entry.subjectKey + "does not exist");
            return;
        }

        string parentKey = info.GetValue<string>("parentKey");

        Transform newParent = GetParent(subject, entry, parentKey);
        Transform oldParent = subject.transform.parent;


        if (oldParent != newParent)
        {
            string debugNextParent = (newParent == null) ? "null" : newParent.name;
            Debug.Log("<color=blue>" + subject.name + " -> " + debugNextParent + "</color>");


            subject.transform.SetParent(newParent);

            //GlobalReposition(info, subject, entry);
            LocalReposition(info, subject);

        }
        else
        {
            if (info.GetValue<long>("duration") == 0)
            {
                Debug.Log("<color=orange>" + subject.name + " First" + "</color>");
            }
            //Debug.Log("Local Reposition");

            LocalReposition(info, subject);
        }
    }

    static void LocalReposition(LogInfo info, GameObject subject)
    {
        Vector3 position = info.GetValue<Vector3>("position");
        Vector3 scale = info.GetValue<Vector3>("scale");
        Quaternion rotation = info.GetValue<Quaternion>("rotation");
        long duration = info.GetValue<long>("duration");

        subject.LocalMoveTo(position, ((float)duration) / 1000f);
        subject.LocalRotateTo(rotation, ((float)duration) / 1000f);
        subject.LocalScaleTo(scale, ((float)duration) / 1000f);
    }

    static void GlobalReposition(LogInfo info, GameObject subject, PlaybackLogEntry entry)
    {
        Vector3 position = info.GetValue<Vector3>("position");
        Vector3 scale = info.GetValue<Vector3>("scale");
        Quaternion rotation = info.GetValue<Quaternion>("rotation");
        long duration = info.GetValue<long>("duration");
        Debug.Log("<color=orange>G:" + " Object " + subject.name + " Location: " + position + " time: " + entry.timeStamp + "parent " + subject.transform.parent + "</color>");

        subject.MoveTo(position, ((float)duration) / 1000f);
        subject.RotateTo(rotation, ((float)duration) / 1000f);
        subject.GlobalScaleTo(scale, ((float)duration) / 1000f);
    }

    static Transform GetParent(GameObject subject, PlaybackLogEntry entry, string parentKey)
    {
        Transform newParentTransform = null;
        GameObject newParent;
        if (PlaybackLogEntry.TryGetObject(parentKey, out newParent))
        {
            newParentTransform = (parentKey == "") ? null : newParent.transform;
        }
        else
        {
            Debug.Log("<color=blue>" + subject.name + " -> " + parentKey + "</color>");
            Debug.LogError(entry.timeStamp + " " + subject.name + " could not reparent because parent " + parentKey + " does not exist.");
        }

        return newParentTransform;
    }
}
