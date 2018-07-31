using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Nanome.Core.Daemon;
using System.Diagnostics;

public class PlaybackClock// : Nanome.Core.Behaviour
{
    public static Stopwatch timer = new Stopwatch();


    public delegate void ClockCallBack();
    private static event ClockCallBack triggerTimer;
    static bool running;


    const int numRot = 7;
    static int thisRot = 0;
    [SerializeField]
    private static List<ClockCallBack> callBackList = new List<ClockCallBack>();

    static int numUpdates = 0;
    static int numFUpdates = 0;

    public static void StartClock()
    {
        UnityEngine.Debug.Log("clock start");
        timer.Start();
        running = true;
        Dispatcher.queue(runTimer());
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
        callBackList = new List<ClockCallBack>();
    }

    public static void RestartClock()
    {
        timer.Reset();
    }

    static long timeLastChecked = 0;

    static bool CheckTimer()
    {
        // long time = GetTime();
        // if (time - timeLastChecked > PlaybackLog.Period)
        // {
        //     timeLastChecked = time;
        //     return true;
        // }
        return false;
    }


    static IEnumerator runTimer()
    {
        while (running)
        {

            int i = thisRot;
            while (i < callBackList.Count)
            {
                callBackList[i].Invoke();
                i += numRot;
            }
            thisRot = (thisRot + 1) % numRot;

            // if (CheckTimer())
            // {
            // UnityEngine.Debug.Log("updates: " + numUpdates);
            // UnityEngine.Debug.Log("fupdates: " + numFUpdates);
            // numUpdates = 0; numFUpdates = 0;

            // if (triggerTimer != null)
            // {
            //     triggerTimer.Invoke();
            // }
            // }
            yield return null;
        }
    }

    static bool timerRunning = false;

    public static void AddToTimer(ClockCallBack callBack)
    {
        callBackList.Add(callBack);
        //triggerTimer += callBack;
    }

    public static void RemoveFromTimer(ClockCallBack callBack)
    {
        callBackList.Remove(callBack);
        // if (callbackRotation == null) return;
        // foreach (List<ClockCallBack> cbl in callbackRotation)
        // {
        //     cbl.Remove(callBack);
        // }

        //triggerTimer -= callBack;
    }

}

