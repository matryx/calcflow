using System.Collections;
using System.Collections.Generic;
using Extensions;
using UnityEngine;
public class ReenactableLoggerTransform : ReenactableLogger
{
    Vector3 lastLocalPos;
    Vector3 lastScale;
    Quaternion lastRotation;
    Transform lastParent;

    bool added = false;

    private void Update()
    {
        if (Recorder.Recording && !added)
        {
            PlaybackClock.AddToTimer(RecordPosition);
            added = true;
        }
    }


    void RecordPosition()
    {
        if (Recorder.Recording)
        {
            if (lastParent != transform.parent || lastLocalPos != transform.localPosition || lastRotation != transform.localRotation || lastScale != transform.localScale)
            {
                GameObject nextParent = (transform.parent == null) ? null : transform.parent.gameObject;
                bool lerp = lastParent == transform.parent;

                long time = PlaybackClock.GetTime() - ((long)PlaybackLog.Period);
                long duration = lerp ? ((long)PlaybackLog.Period) : 0;
                Recorder.RecordAction(PlaybackLogEntry.PlayBackActionFactory.CreateMovement(time, duration, gameObject, transform.localPosition,
                                                                                         transform.localRotation, transform.localScale, nextParent));

                lastParent = transform.parent;
                lastLocalPos = transform.localPosition;
                lastRotation = transform.localRotation;
                lastScale = transform.localScale;
            }
        }
    }


    void OnDisable()
    {
        if (Recorder.Recording)
        {
            if (!gameObject.activeSelf)
            {
                long time = PlaybackClock.GetTime();
                Recorder.RecordAction(PlaybackLogEntry.PlayBackActionFactory.CreateDisable(time, gameObject));
                Debug.Log("logged Disable");
            }
        }
    }

    void OnEnable()
    {

        if (Recorder.Recording)
        {
            if (gameObject.activeSelf)
            {
                long time = PlaybackClock.GetTime();
                Recorder.RecordAction(PlaybackLogEntry.PlayBackActionFactory.CreateEnable(time, gameObject));
            }
        }
    }
    public override void OnDestroy()
    {
        if (Recorder.Recording)
        {
            long time = PlaybackClock.GetTime() + (long)(PlaybackLog.Period * 0.1f);
            Recorder.RecordAction(PlaybackLogEntry.PlayBackActionFactory.CreateDestroy(time, gameObject));
        }
        PlaybackClock.RemoveFromTimer(RecordPosition);
        base.OnDestroy();
    }
}