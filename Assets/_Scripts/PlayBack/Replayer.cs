using System;
using System.Collections;
using System.Collections.Generic;
using Nanome.Core;
using UnityEngine;
using System.Diagnostics;
using System.IO;

public static class Replayer
{

    public static List<PlaybackLogEntry> log;
    private static bool replaying = false;
    public static bool Replaying { get { return replaying; } }

    static string GetFileName()
    {
        string filename = "testRecording";
        string path = System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments);
        string folder = Path.Combine(path, "Recordings");
        return Path.Combine(folder, filename);
    }

    static void LoadReplay(string file)
    {
        using (StreamReader sr = new StreamReader(file + ".txt"))
        {
            string json = sr.ReadToEnd();

            log = JsonUtility.FromJson<PlaybackLog>(json).GetLogCopy();

        }
    }

    public static void StartReplaying()
    {
        Async.runInCoroutine(StartUpProcess);
    }
    static IEnumerator StartUpProcess(Async routine)
    {
        LoadReplay(GetFileName());
        LoadingScreen loadingScreen = StartLoadingScreen();
        loadingScreen.SetBarLimit(100);
        loadingScreen.SetRemaining(100);
        LoggerManager.SetupReenactors();
        yield return null;
        PreLoad();
        yield return null;

        EndLoadingScreen(loadingScreen);
        replaying = true;
        PlaybackClock.RestartClock();
        PlaybackClock.StartClock();
        Async.runInCoroutine(ReplayFromLog);
    }
    static string LoadingScreenPrefab = "Prefabs\\LoadingScreen";
    static LoadingScreen StartLoadingScreen()
    {
        GameObject LoadingScreen = GameObject.Instantiate(Resources.Load(LoadingScreenPrefab, typeof(GameObject))) as GameObject;
        LoadingScreen ls = LoadingScreen.GetComponent<LoadingScreen>();
        ls.StartLoading();
        return ls;
    }
    static void EndLoadingScreen(LoadingScreen loadingScreen)
    {
        loadingScreen.StopLoading();
        GameObject.Destroy(loadingScreen.gameObject);
    }

    private static void PreLoad()
    {

        while (true)
        {
            if (log.Count == 0)
            {
                UnityEngine.Debug.Log("replay finished");

                StopReplaying();
                break;
            }
            if (log[0].timeStamp <= 0)
            {
                PlaybackLogEntry item = log[0];
                log.RemoveAt(0);
                item.Reenact();
                // try
                // {
                //     item.Reenact();
                // }
                // catch (Exception e)
                // {
                //     UnityEngine.Debug.LogError(e.Message);
                // }
            }
            else
            {
                UnityEngine.Debug.Log("<color=yellow>preLoad Finished</color>");
                break;
            }
        }
    }

    private static IEnumerator ReplayFromLog(Async process)
    {

        while (true)
        {
            //print("attempting pop");
            if (log.Count == 0)
            {
                UnityEngine.Debug.Log("replay finished");

                //print("nothing to pop");
                StopReplaying();
                break;
            }
            if (log[0].timeStamp <= PlaybackClock.GetTime())
            {
                //print("popping next instruction");
                PlaybackLogEntry item = log[0];
                log.RemoveAt(0);
                item.Reenact();

                // try
                // {
                //     item.Reenact();
                // }
                // catch (Exception e)
                // {
                //     UnityEngine.Debug.LogError(e.Message);
                // }
            }
            else
            {
                //UnityEngine.Debug.Log("<color=yellow>Breaking for now:</color>");
                yield return null;
            }
        }
    }

    public static void StopReplaying()
    {
        PlaybackClock.StopClock();
        replaying = false;
    }

    public static void PauseReplaying()
    {
        PlaybackClock.StopClock();
        replaying = false;
    }
    public static void ResumeReplaying()
    {
        PlaybackClock.StartClock();
        replaying = true;
    }

}