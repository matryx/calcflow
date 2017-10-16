using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Recorder : MonoBehaviour {

    public bool EditorRecord = false;

    private static bool record = false;

    private void Update()
    {
        Recording = EditorRecord;
        if (Recording) print("recording...");
    }

    private static void StartRecording()
    {
        PlaybackClock.StartClock();
    }

    private static void StopRecording()
    {
        PlaybackClock.StopClock();
    }

    public static bool Recording
    {
        get
        {
            return record;
        } set
        {
            if (value && !record)
            {
                StartRecording();
            }
            if (!value && record)
            {
                StopRecording();
            }
            record = value;
        }
    }
}
