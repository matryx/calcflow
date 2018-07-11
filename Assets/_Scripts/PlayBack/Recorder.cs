using System.Collections;
using System.Collections.Generic;
using CalcFlowUI;
using Extensions;
using Nanome.Core;
using Nanome.Core.Daemon;
using UnityEditor;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

public static class Recorder
{
    public static GameObject _instanceGO;

    public static string SavedLog;
    private static HashSet<int> AllGameObjects = new HashSet<int>();
    private static List<UIDSystem> allUIDs = new List<UIDSystem>();
    //DEBUGGING.
    public static int SpawnQueueSize()
    {
        return allUIDs.Count;
    }
    private static bool recording;
    public static bool Recording { get { return recording; } }
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

    static int recsPerFrame = 5;

    private static IEnumerator PreSave2(Async routine)
    {
        LoadingScreen loadingScreen = StartLoadingScreen();
        yield return null;
        int numRecs = 0;
        while (allUIDs.Count > 0)
        {
            while (numRecs < recsPerFrame && allUIDs.Count > 0)
            {
                numRecs++;
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
                    Debug.Log("uid was deleted");
                }
            }
            numRecs = 0;
            yield return null;
        }
        Debug.Log("preRecording finished");
        routine.pushEvent("SaveComplete", loadingScreen);

    }

    private static void StartUpProcess(object loadingScreen)
    {
        LoggerManager.SetupReenactors();
        EndLoadingScreen((LoadingScreen)loadingScreen);
        Debug.Log("startingClock");
        PlaybackClock.StartClock();
        PlaybackClock.AddToTimer(CheckForSpawns);
        recording = true;
        paused = false;
    }
    #region loadingScreenStuff
    static string LoadingScreenPrefab = "Prefabs\\LoadingScreen";
    static LoadingScreen StartLoadingScreen()
    {
        GameObject LoadingScreen = GameObject.Instantiate(Resources.Load(LoadingScreenPrefab, typeof(GameObject))) as GameObject;
        return LoadingScreen.GetComponent<LoadingScreen>();
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
        recording = true;
        paused = false;
        Async startup = Async.runInCoroutine(PreSave2);
        startup.onEvent("SaveComplete", StartUpProcess);
    }
    public static void PauseRecording()
    {
        Debug.Log("paused recording");
        PlaybackClock.StopClock();
        PlaybackClock.RemoveFromTimer(CheckForSpawns);
        paused = true;
    }
    public static void ResumeRecording()
    {
        Debug.Log("resumed recording");
        PlaybackClock.StartClock();
        PlaybackClock.AddToTimer(CheckForSpawns);
        paused = false;
    }
    public static void EndRecording()
    {
        Debug.Log("stop recording");
        PlaybackClock.StopClock();
        PlaybackClock.RemoveFromTimer(CheckForSpawns);
        SavedLog = JsonUtility.ToJson(recordLog);
        recording = false;
        paused = false;
    }

    public static void UIDAdded(UIDSystem uid)
    {
        allUIDs.Add(uid);
        if (Recording)
        {
            RecordSpawn(uid);
        }
    }

    public static void AddUID(UIDSystem uid)
    {
        allUIDs.Add(uid);
    }

    private static void RecordSpawn(UIDSystem uid)
    {
        GameObject gObj = uid.gameObject;
        LogSpawn(gObj);
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
                Debug.Log("uid was deleted");
            }
        }
    }
    #region logCode
    public static void LogAction(PlaybackLogEntry entry)
    {
        recordLog.log.Add(entry);
    }

    public static void LogSpawn(GameObject subject)
    {
        long time = PlaybackClock.GetTime() - ((long)PlaybackLog.Period * 1005);
        recordLog.log.Add(PlaybackLogEntry.PlayBackActionFactory.CreateSpawn(time,
            subject,
            subject.transform.position,
            subject.transform.rotation,
            subject.transform.lossyScale));
    }

    public static void LogMovement(GameObject subject, Vector3 destination, Quaternion rotation, Vector3 scale, GameObject parent, bool useLerp)
    {
        long time = PlaybackClock.GetTime() - ((long)PlaybackLog.Period * 1000);
        long duration = useLerp ? ((long)PlaybackLog.Period * 1000) : 0;
        recordLog.log.Add(PlaybackLogEntry.PlayBackActionFactory.CreateMovement(time, duration, subject, destination, rotation, scale, parent));
    }

    public static void LogEnable(GameObject subject)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogEntry.PlayBackActionFactory.CreateEnable(time, subject));
    }

    public static void LogDisable(GameObject subject)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogEntry.PlayBackActionFactory.CreateDisable(time, subject));
    }

    public static void LogDestroy(GameObject subject)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogEntry.PlayBackActionFactory.CreateDestroy(time, subject));
    }

    public static void LogButtonPress(GameObject subject, GameObject presser)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogEntry.PlayBackActionFactory.CreateButtonPress(time, subject, presser));
    }

    public static void LogButtonUnpress(GameObject subject, GameObject presser)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogEntry.PlayBackActionFactory.CreateButtonUnpress(time, subject, presser));
    }
    #endregion
}