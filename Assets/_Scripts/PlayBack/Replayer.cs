using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Replayer : MonoBehaviour {

    public bool EditorReplay = false;
    private static List<PlaybackLog.PlayBackLogAction> log;
    private static bool replay = false;
    private static PlaybackClock clock = new PlaybackClock();

    public static Replayer _instance;

    private void Start()
    {
        _instance = this;
        LoadReplay(PlaybackLog.recordingInstance);
    }

    private void Update()
    {
        Replaying = EditorReplay;
    }
    
    private void LoadReplay(PlaybackLog replay)
    {
        log = replay.GetLogCopy();
    }

    private static void StartReplaying()
    {
        _instance.LoadReplay(PlaybackLog.recordingInstance);
        PlaybackClock.StartClock();
    }

    private void FixedUpdate()
    {
        if (Replaying)
        {
            while (true)
            {
                print("attempting pop");
                if (log.Count == 0)
                {
                    print("nothing to pop");
                    Replaying = false;
                    break;
                }
                if (log[0].timeStamp <= PlaybackClock.GetTime())
                {
                    print("popping next instruction");
                    log[0].Reenact();
                    log.RemoveAt(0);
                }
                else
                {
                    print("nothing to pop... yet");
                    break;
                }
            }
        }
    }

    private static void StopReplaying()
    {
        PlaybackClock.StopClock();
        _instance.EditorReplay = false;
        replay = false;
    }

    public static bool Replaying
    {
        get
        {
            return replay;
        }
        set
        {
            if (value && !replay)
            {
                StartReplaying();
            }
            if (!value && replay)
            {
                StopReplaying();
            }
            replay = value;
        }
    }
}
