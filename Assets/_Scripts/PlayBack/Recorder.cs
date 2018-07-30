using System.Collections;
using System.Collections.Generic;
using CalcFlowUI;
using Extensions;
using Nanome.Core;
using Nanome.Core.Daemon;
using UnityEditor;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;
using System.Diagnostics;


public static class Recorder
{
    public static Stopwatch timer = new Stopwatch();

    public static GameObject _instanceGO;

    public static string SavedLog;
    private static HashSet<int> AllGameObjects = new HashSet<int>();
    private static List<UIDSystem> allUIDs = new List<UIDSystem>();
    public static int SpawnQueueSize()
    {
        return allUIDs.Count;
    }
    private static bool recording = false;
    public static bool Recording { get { return recording && !paused; } }
    private static bool paused = false;
    public static bool Paused { get { return paused; } }

    [SerializeField]
    public static PlaybackLog recordLog = new PlaybackLog();

    private static List<UIDSystem> GetAllUIDSInScene()
    {
        List<UIDSystem> objectsInScene = new List<UIDSystem>();

        foreach (UIDSystem go in Resources.FindObjectsOfTypeAll(typeof(UIDSystem)) as UIDSystem[])
        {
            if (go.gameObject.hideFlags == HideFlags.NotEditable || go.gameObject.hideFlags == HideFlags.HideAndDontSave)
                continue;
            if (go.gameObject.scene.name == null)
                continue;

            objectsInScene.Add(go);
        }

        return objectsInScene;
    }


    private static IEnumerator PreSave(Async routine)
    {
        LoadingScreen loadingScreen = StartLoadingScreen();
        loadingScreen.SetBarLimit(allUIDs.Count);
        loadingScreen.SetRemaining(allUIDs.Count);
        yield return null;
        bool saved = false;
        while (allUIDs.Count > 0)
        {
            while (!saved && allUIDs.Count > 0)
            {
                UIDSystem uid;
                uid = allUIDs[allUIDs.Count - 1];
                allUIDs.RemoveAt(allUIDs.Count - 1);
                if (uid)
                {
                    LoggerManager.SetupLoggers(uid.gameObject);
                    saved = SaveObject(uid);
                }
                else
                {
                    UnityEngine.Debug.Log("uid was deleted");
                }
            }
            loadingScreen.SetRemaining(allUIDs.Count);
            saved = false;
            yield return null;
        }
        UnityEngine.Debug.Log("preRecording finished");
        routine.pushEvent("SaveComplete", loadingScreen);

    }

    private static void StartUpProcess(object loadingScreen)
    {
        timer.Stop();
        UnityEngine.Debug.Log("SaveTime: " + timer.Elapsed);
        EndLoadingScreen((LoadingScreen)loadingScreen);
        UnityEngine.Debug.Log("startingClock");
        PlaybackClock.RestartClock();
        PlaybackClock.StartClock();
        recording = true;
        PlaybackClock.AddToTimer(CheckForSpawns);
        paused = false;
    }
    #region loadingScreenStuff
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
    #endregion
    public static void StartRecording()
    {
        allUIDs.Clear();
        allUIDs = GetAllUIDSInScene();
        paused = false;
        Async startup = Async.runInCoroutine(PreSave);
        startup.onEvent("SaveComplete", StartUpProcess);
        timer.Start();
    }
    public static void PauseRecording()
    {
        UnityEngine.Debug.Log("paused recording");
        PlaybackClock.StopClock();
        PlaybackClock.RemoveFromTimer(CheckForSpawns);
        paused = true;
    }
    public static void ResumeRecording()
    {
        UnityEngine.Debug.Log("resumed recording");
        PlaybackClock.StartClock();
        PlaybackClock.AddToTimer(CheckForSpawns);
        paused = false;
    }
    public static void EndRecording()
    {
        UnityEngine.Debug.Log("stop recording");
        PlaybackClock.StopClock();
        PlaybackClock.RemoveFromTimer(CheckForSpawns);
        SavedLog = JsonUtility.ToJson(recordLog);
        recording = false;
        paused = false;
    }

    public static void AddUID(UIDSystem uid)
    {
        allUIDs.Add(uid);
    }

    private static void CheckForSpawns()
    {
        while (allUIDs.Count > 0)
        {
            UIDSystem uid;
            uid = allUIDs[allUIDs.Count - 1];
            allUIDs.RemoveAt(allUIDs.Count - 1);
            if (uid)
            {
                LoggerManager.SetupLoggers(uid.gameObject);
                RecordSpawn(uid);
            }
            else
            {
                UnityEngine.Debug.Log("uid was deleted");
            }
        }
    }
    #region logCode
    public static void LogAction(PlaybackLogEntry entry)
    {
        recordLog.log.Add(entry);
    }
    private static void RecordSpawn(UIDSystem uid)
    {
        GameObject gObj = uid.gameObject;
        LogSpawn(gObj);
    }

    private static bool SaveObject(UIDSystem uid)
    {
        GameObject gObj = uid.gameObject;
        if (gObj.transform.parent == null)
        {
            LogSpawn(gObj);
            return true;
        }
        else { return false; };
    }

    public static void LogSpawn(GameObject subject)
    {
        long time = PlaybackClock.GetTime() - (long)(PlaybackLog.Period * 1.05f);
        recordLog.log.Add(PlaybackLogEntry.PlayBackActionFactory.CreateSpawn(time,
            subject,
            subject.transform.position,
            subject.transform.rotation,
            subject.transform.lossyScale));
    }
    #endregion
}