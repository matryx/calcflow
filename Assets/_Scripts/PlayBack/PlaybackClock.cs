using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Diagnostics;

public class PlaybackClock : MonoBehaviour{

    public Stopwatch timer = new Stopwatch();

    public void StartClock()
    {
        timer.Start();
    }

    public long GetTime()
    {
        return timer.ElapsedMilliseconds;
    }

    public void StopClock()
    {
        timer.Stop();
    }

    public void RestartClock()
    {
        timer.Reset();
    }
}
