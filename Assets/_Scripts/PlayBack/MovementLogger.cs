using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MovementLogger : Nanome.Core.Behaviour
{
    Vector3 lastLocalPos;
    Vector3 lastScale;
    Quaternion lastRotation;

    private void Start()
    {
        PlaybackClock.AddToTimer(RecordPosition);
    }

    void RecordPosition()
    {
        if (lastLocalPos != transform.localPosition || lastRotation != transform.rotation || lastScale != transform.lossyScale)
        {
            lastLocalPos = transform.localPosition;
            lastRotation = transform.rotation;
            lastScale = transform.lossyScale;
            Recorder.LogMovement(gameObject, transform.localPosition, transform.rotation, transform.lossyScale);
        }
    }
}
