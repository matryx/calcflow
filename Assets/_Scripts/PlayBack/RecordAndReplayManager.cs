using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RecordAndReplayManager : MonoBehaviour
{
    public bool EditorRecord = false;
    public bool EditorPause = false;
    public bool EditorReplay = false;

    public static bool Recording
    {
        get
        {
            return Recorder.Recording && !Recorder.Paused;
        }
        set
        {
            if (value && !Recorder.Recording)
            {
                Recorder.StartRecording();
            }
            else if (!value && Recorder.Recording)
            {
                Recorder.EndRecording();
            }
        }
    }
    public static bool Paused
    {
        get
        {
            return Recorder.Paused;
        }
        set
        {
            if (value && !Recorder.Paused)
            {
                Recorder.PauseRecording();
            }
            else if (!value && Recorder.Paused)
            {
                Recorder.ResumeRecording();
            }
        }
    }

    public static bool Replaying
    {
        get
        {
            return Replaying;
        }
        set
        {
            if (value && !Replayer.Replaying)
            {
                Replayer.StartReplaying();
            }
            if (!value && Replayer.Replaying)
            {
                Replayer.StopReplaying();
            }
        }
    }

    void Update()
    {
        Paused = EditorPause;
        Recording = EditorRecord;
        Replaying = EditorReplay;

    }
}
