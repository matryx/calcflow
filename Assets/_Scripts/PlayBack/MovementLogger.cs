using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MovementLogger : Nanome.Core.Behaviour
{

    Vector3 lastPos;
    Vector3 lastScale;
    Quaternion lastRotation;

    float timeSinceLastCheck = 0.0f;

    void RecordPosition()
    {
        timeSinceLastCheck += Time.fixedDeltaTime;
        print(PlaybackLog.Period);
        if (timeSinceLastCheck > PlaybackLog.Period)
        {
            if (lastPos != transform.position || lastRotation != transform.rotation || lastScale != transform.lossyScale)
            {
                lastPos = transform.position;
                lastRotation = transform.rotation;
                lastScale = transform.lossyScale;
                PlaybackLog.LogMovement(gameObject, transform.position, transform.rotation, transform.lossyScale);
            }
            timeSinceLastCheck = 0.0f;
        }
    }

    private void FixedUpdate()
    {
        if (Recorder.Recording)
        {
            RecordPosition();
        }
    }
}
