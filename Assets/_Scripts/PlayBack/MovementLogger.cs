using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MovementLogger : Nanome.Core.Behaviour
{
    Vector3 lastLocalPos;
    Vector3 lastScale;
    Quaternion lastRotation;
    Transform lastParent;

    private void Start()
    {
        if(Recorder.Recording)
            PlaybackClock.AddToTimer(RecordPosition);
    }

    void RecordPosition()
    {
        if (lastParent != transform.parent || lastLocalPos != transform.localPosition || lastRotation != transform.localRotation || lastScale != transform.localScale)
        {
            lastParent = transform.parent;
            lastLocalPos = transform.localPosition;
            lastRotation = transform.localRotation;
            lastScale = transform.localScale;

            GameObject nextParent;
            if (transform.parent == null)
            {
                nextParent = null;
            }
            else
            {
                nextParent = transform.parent.gameObject;
            }

            Recorder.LogMovement(gameObject, lastLocalPos, lastRotation, lastScale, nextParent);
        }
    }

    private void OnDestroy()
    {
        PlaybackClock.RemoveFromTimer(RecordPosition);
    }
}
