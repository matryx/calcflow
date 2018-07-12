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

    private void Start()
    {

        if (Recorder.Recording)
        {
            PlaybackClock.AddToTimer(RecordPosition);
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

                long time = PlaybackClock.GetTime() - ((long)PlaybackLog.Period * 1000);
                long duration = lerp ? ((long)PlaybackLog.Period * 1000) : 0;
                Recorder.LogAction(PlaybackLogEntry.PlayBackActionFactory.CreateMovement(time, duration, gameObject, transform.localPosition,
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
        if (Replayer.Replaying)
        {
            long time = PlaybackClock.GetTime();
            Recorder.LogAction(PlaybackLogEntry.PlayBackActionFactory.CreateDisable(time, gameObject));
        }
    }

    void OnEnable()
    {
        if (Replayer.Replaying)
        {
            long time = PlaybackClock.GetTime();
            Recorder.LogAction(PlaybackLogEntry.PlayBackActionFactory.CreateEnable(time, gameObject));
        }
    }
    public override void OnDestroy()
    {
        if (Replayer.Replaying)
        {
            long time = PlaybackClock.GetTime();
            Recorder.LogAction(PlaybackLogEntry.PlayBackActionFactory.CreateDestroy(time, gameObject));
        }
        PlaybackClock.RemoveFromTimer(RecordPosition);
        base.OnDestroy();
    }
}