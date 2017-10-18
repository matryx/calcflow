using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Recorder : MonoBehaviour {

    public bool EditorRecord = false;
    public static PlaybackClock clock = new PlaybackClock();

    private static bool record = false;

    private void Start()
    {
    }

    private void Update()
    {
        Recording = EditorRecord;
        if (Recording) print("recording...");
    }

    private static void StartRecording()
    {
        
        clock.StartClock();
    }

    private static void StopRecording()
    {
        clock.StopClock();
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
