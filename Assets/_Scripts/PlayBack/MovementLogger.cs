using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MovementLogger : Nanome.Core.Behaviour
{

    Vector3 lastPos;
    float timeSinceLastCheck = 0.0f;

    void RecordPosition()
    {
        timeSinceLastCheck += Time.fixedDeltaTime;
        print(PlaybackLog.Period);
        if (timeSinceLastCheck > PlaybackLog.Period)
        {
            if (lastPos != transform.position)
            {
                lastPos = transform.position;
                PlaybackLog.LogMovement(Recorder.clock.GetTime()-(long)(PlaybackLog.Period*1000), gameObject, transform.position);
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
