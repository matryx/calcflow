using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Replayer : MonoBehaviour {

    public bool EditorReplay = false;
    public List<PlaybackLog.PlayBackLogAction> log;
    private static bool replay = false;

    private void Start()
    {
        LoadReplay(PlaybackLog.instance);
    }

    private void Update()
    {
        Replaying = EditorReplay;
    }
    
    private void LoadReplay(PlaybackLog replay)
    {
        this.log = replay.GetLogCopy();
    }

    private void StartReplaying()
    {
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
                if (log[0].timeStamp <= PlaybackClock.GetTime() - PlaybackLog.Period)
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

    private void StopReplaying()
    {
        PlaybackClock.StopClock();
        EditorReplay = false;
        replay = false;
    }

    public bool Replaying
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
