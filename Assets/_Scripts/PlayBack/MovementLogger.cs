using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MovementLogger : PlayBackLogger
{
    Vector3 lastLocalPos;
    Vector3 lastScale;
    Quaternion lastRotation;
    Transform lastParent;

    private void Start()
    {
        if (Recorder.Recording)
            PlaybackClock.AddToTimer(RecordPosition);
    }

    void RecordPosition()
    {
        if (lastParent != transform.parent || lastLocalPos != transform.localPosition || lastRotation != transform.localRotation || lastScale != transform.localScale)
        {
            bool lerp = lastParent == transform.parent;
            lastParent = transform.parent;
            lastLocalPos = transform.localPosition;
            lastRotation = transform.localRotation;
            lastScale = transform.localScale;

            GameObject nextParent = (transform.parent == null) ? null : transform.parent.gameObject;

            Recorder.LogMovement(gameObject, lastLocalPos, lastRotation, lastScale, nextParent, lerp);
        }
    }
    public  Dictionary<string, PlaybackLogAction2.ReenactAction> GetReenactors(){
        print("suh");
        return new Dictionary<string, PlaybackLogAction2.ReenactAction>();
    }
    public override void OnDestroy()
    {
        base.OnDestroy();
        PlaybackClock.RemoveFromTimer(RecordPosition);
    }
}
