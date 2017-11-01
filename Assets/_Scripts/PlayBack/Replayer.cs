using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Replayer : MonoBehaviour {

    public bool EditorReplay = false;
    private static List<PlayBackLogAction> log;
    private static bool replay = false;

    public static Replayer _instance;

    private void Start()
    {
        _instance = this;
    }

    private void Update()
    {
        Replaying = EditorReplay;
    }
    
    private void LoadReplay(string json)
    {
        LoadReplay(JsonUtility.FromJson<PlaybackLog>(json));
    }

    private void LoadReplay(PlaybackLog replay)
    {
        log = replay.GetLogCopy();
    }

    private static void StartReplaying()
    {
        _instance.LoadReplay(JsonUtility.ToJson(Recorder.recordLog));
        PlaybackClock.RestartClock();
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
                    PlayBackLogAction item = log[0];
                    log.RemoveAt(0);
                    item.Reenact();
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
