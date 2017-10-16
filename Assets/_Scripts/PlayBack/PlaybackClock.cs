using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Diagnostics;

public static class PlaybackClock{

    static Stopwatch timer = new Stopwatch();

    public static void StartClock()
    {
        timer.Start();
    }

    public static long GetTime()
    {
        return timer.ElapsedMilliseconds;
    }

    public static void StopClock()
    {
        timer.Stop();
        timer.Reset();
    }
}
