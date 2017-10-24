using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MovementLogger : Nanome.Core.Behaviour
{
    Vector3 lastPos;
    Vector3 lastScale;
    Quaternion lastRotation;

    private void Start()
    {
        PlaybackClock.AddToTimer(RecordPosition);
    }

    void RecordPosition()
    {
        if (lastPos != transform.position || lastRotation != transform.rotation || lastScale != transform.lossyScale)
        {
            lastPos = transform.position;
            lastRotation = transform.rotation;
            lastScale = transform.lossyScale;
            Recorder.LogMovement(gameObject, transform.position, transform.rotation, transform.lossyScale);
        }
    }
}
