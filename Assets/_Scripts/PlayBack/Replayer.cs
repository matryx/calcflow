using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Replayer : MonoBehaviour {

    public bool EditorReplay = false;
    public List<PlaybackLog.PlayBackLogAction> log;
    private static bool replay = false;

    private void Start()
    {
        log = PlaybackLog.Log;
    }

    private void Update()
    {
        Replaying = EditorReplay;
        if (Replaying) print("replaying...");
    }

    private static void StartReplaying()
    {
        PlaybackClock.StartClock();
    }

    private void FixedUpdate()
    {
        if (Replaying)
        {
            while (true)
            {
                if (log.Count == 0)
                {
                    Replaying = false;
                    break;
                }
                if (log[0].timeStamp < PlaybackClock.GetTime())
                {
                    log[0].Reenact();
                    log.RemoveAt(0);
                }
                else
                {
                    break;
                }
            }
        }
    }

    private static void StopReplaying()
    {
        PlaybackClock.StopClock();
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
