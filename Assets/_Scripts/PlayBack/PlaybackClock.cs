using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Nanome.Core.Daemon;
using System.Diagnostics;

public class PlaybackClock// : Nanome.Core.Behaviour
{
    public static Stopwatch timer = new Stopwatch();
    static bool running;
    [SerializeField]
    public static void StartClock()
    {
        UnityEngine.Debug.Log("clock start");
        timer.Start();
        running = true;
    }

    public static long GetTime()
    {
        if (!running) return -777;
        return timer.ElapsedMilliseconds;
    }

    public static void StopClock()
    {
        UnityEngine.Debug.Log("clock stop");
        running = false;
        timer.Stop();
    }

    public static void RestartClock()
    {
        timer.Reset();
    }

}

