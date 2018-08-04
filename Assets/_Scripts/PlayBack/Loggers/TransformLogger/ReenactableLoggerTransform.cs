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

    long lastTime = -999;

    void Awake()
    {
        if (Recorder.Recording)
        {
            UnityEngine.Debug.Log("move first rec frame: " + PBT_FrameCounter.GetFrame());

            long currTime = PlaybackClock.GetTime(); ;
            LogMovement(currTime);
        }
    }

    void Update()
    {
        if (Recorder.Recording)
        {
            RecordPosition();
        }
    }

    void RecordPosition()
    {
        if (Recorder.Recording)
        {
            if (Changed())
            {
                long currTime = PlaybackClock.GetTime(); ;
                LogMovement(currTime);
            }
        }
    }

    // returns true if any of the transform paramteres have changed since last check.
    bool Changed()
    {
        bool changed = false;

        changed |= lastParent != transform.parent;
        changed |= lastLocalPos != transform.localPosition;
        changed |= lastRotation != transform.localRotation;
        changed |= lastScale != transform.localScale;

        return changed;
    }

    void LogMovement(long currTime)
    {
        long startTime;
        long endTime;
        long duration;


        GameObject nextParent = (transform.parent == null) ? null : transform.parent.gameObject;
        bool lerp = true;
        bool world = false;

        if (lastTime == -999)
        {
            lerp = false;
        }
        if (lastParent != transform.parent)
        {
            //string debugLastParent = (lastParent == null) ? "null" : PlaybackLogEntry.GetUniqueID(lastParent.gameObject);
            string debugNextParent = (transform.parent == null) ? "null" : PlaybackLogEntry.GetUniqueID(transform.parent.gameObject);
            Debug.Log("<color=blue>" + gameObject.name + " -> " + debugNextParent + "</color>");

            world = true;
        }

        if (lerp)
        {
            startTime = lastTime;
            endTime = currTime;
            duration = endTime - startTime;
        }
        else
        {
            startTime = currTime;
            endTime = currTime;
            duration = 0;
        }

        if (world)
        {
            Recorder.RecordAction(PlaybackLogEntry.PlayBackActionFactory.CreateMovement(startTime, duration, gameObject, transform.position,
                                                                                     transform.rotation, transform.lossyScale, nextParent));
        }
        else
        {
            Recorder.RecordAction(PlaybackLogEntry.PlayBackActionFactory.CreateMovement(startTime, duration, gameObject, transform.localPosition,
                                                                                     transform.localRotation, transform.localScale, nextParent));
        }


        UpdateHistory(currTime);
    }

    private void UpdateHistory(long currTime)
    {
        lastParent = transform.parent;
        lastLocalPos = transform.localPosition;
        lastRotation = transform.localRotation;
        lastScale = transform.localScale;
        lastTime = currTime;
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