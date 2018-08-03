using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RecordAndReplayManager : MonoBehaviour
{
    public static RecordAndReplayManager _instance;
    public bool EditorRecord = false;
    public bool EditorPause = false;
    public bool EditorReplay = false;
    private static bool recording = false;
    public static bool Recording
    {
        get
        {
            return recording;
        }
        set
        {
            if (value && !recording)
            {
                Recorder.StartRecording();
            }
            else if (!value && recording)
            {
                Recorder.EndRecording();
            }
            recording = value;
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
    private static bool replaying = false;
    public static bool Replaying
    {
        get
        {
            return Replayer.Replaying;
        }
        set
        {
            if (value && !replaying)
            {
                Replayer.StartReplaying();
            }
            if (!value && replaying)
            {
                Replayer.StopReplaying();
            }
            replaying = value;
        }
    }

    void Awake()
    {
        _instance = this;
    }

    void Update()
    {
        Paused = EditorPause;
        Recording = EditorRecord;
        Replaying = EditorReplay;

    }
}
