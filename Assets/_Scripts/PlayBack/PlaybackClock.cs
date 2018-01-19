using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Nanome.Core.Daemon;
using System.Diagnostics;

public class PlaybackClock : Nanome.Core.Behaviour
{
    public static Stopwatch timer = new Stopwatch();

    public delegate void ClockCallBack();
    private static event ClockCallBack triggerTimer;
    static bool running;

    public static void StartClock()
    {
        timer.Start();
        running = true;
        Dispatcher.queue(runTimer());
    }

    public static long GetTime()
    {
        return timer.ElapsedMilliseconds;
    }

    public static void StopClock()
    {
        running = false;
        timer.Stop();
    }

    public static void RestartClock()
    {
        timer.Reset();
    }

    static long timeLastChecked = 0;

    static bool CheckTimer()
    {
        long time = GetTime();
        if (time - timeLastChecked > PlaybackLog.Period)
        {
            timeLastChecked = time;
            return true;
        }
        return false;
    }


    static IEnumerator runTimer()
    {
        while (running)
        {
            if (CheckTimer())
            {
                if (triggerTimer != null)
                    triggerTimer.Invoke();
            }
            yield return null;
        }
    }

    static bool timerRunning = false;

    public static void AddToTimer(ClockCallBack callBack)
    {
        triggerTimer += callBack;
    }

    public static void RemoveFromTimer(ClockCallBack callBack)
    {
        triggerTimer -= callBack;
    }

}

