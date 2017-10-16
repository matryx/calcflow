using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MovementLogger : Nanome.Core.Behaviour
{

    Vector3 lastPos;
    float timeSinceLastCheck = 0.0f;
    float checkFrequency = .03f;

    void RecordPosition()
    {
        timeSinceLastCheck += Time.fixedDeltaTime;
        if (timeSinceLastCheck > checkFrequency)
        {
            if (lastPos != transform.position)
            {
                lastPos = transform.position;
                PlaybackLog.LogMovement(PlaybackClock.GetTime(), gameObject, transform.position);
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
