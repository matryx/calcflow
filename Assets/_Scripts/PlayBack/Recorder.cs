using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using Extensions;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;
using CalcFlowUI;


public class Recorder : MonoBehaviour
{

    public static Recorder _instance;
    public static GameObject _instanceGO;

    public bool EditorRecord = false;
    private static HashSet<int> AllGameObjects = new HashSet<int>();
    private static List<UIDSystem> allUIDs = new List<UIDSystem>();
    private static bool record = false;
    [SerializeField]
    public static PlaybackLog2 recordLog = new PlaybackLog2();

    private void Start()
    {
        if (_instance == null)
        {
            _instance = this;
            _instanceGO = this.gameObject;
        }
        else
        {
            Debug.LogWarning("Two instances of one-of: Recorder");
        }
        allUIDs.Clear();
        allUIDs = GetAllUIDSInScene();
        allUIDs.Remove(gameObject.GetComponent<UIDSystem>());
    }

    List<UIDSystem> GetAllUIDSInScene()
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

    private void Update()
    {
        Recording = EditorRecord;
    }

    private static void StartRecording()
    {
        print("start recording");
        SetupLoggers();
        CheckForSpawns();
        print("preRecording finished");
        PlaybackClock.StartClock();
        PlaybackClock.AddToTimer(CheckForSpawns);
    }
    private static void SetupLoggers()
    {
        LoggerManager.SetupLoggers();
    }

    private static void StopRecording()
    {
        print("stop recording");
        PlaybackClock.StopClock();

        PlaybackClock.RemoveFromTimer(CheckForSpawns);
    }

    public static bool Recording
    {
        get
        {
            return record;
        }
        set
        {
            if (value && !record)
            {
                StartRecording();
            }
            if (!value && record)
            {
                StopRecording();
            }
            record = value;
        }
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
        if (_instance != null)
        {
            allUIDs.Add(uid);
        }
    }

    private static void RecordSpawn(UIDSystem uid)
    {
        GameObject gObj = uid.gameObject;
        LogSpawn(gObj);
        if (gObj.GetComponent<Button>() != null)
        {
            gObj.EnsureOneOf<ButtonLogger>();
        }
        gObj.EnsureOneOf<EnableLogger>();
        gObj.EnsureOneOf<MovementLogger>();
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
                RecordSpawn(uid);
            }
            else
            {
                Debug.Log("uid was deleted");
            }
        }
    }

    public static void LogSpawn(GameObject subject)
    {
        long time = PlaybackClock.GetTime() - ((long)PlaybackLog2.Period * 1005);
        recordLog.log.Add(PlaybackLogAction2.PlayBackActionFactory.CreateSpawn(time,
                                                            subject,
                                                            subject.transform.position,
                                                            subject.transform.rotation,
                                                            subject.transform.lossyScale));
    }

    public static void LogMovement(GameObject subject, Vector3 destination, Quaternion rotation, Vector3 scale, GameObject parent, bool useLerp)
    {
        long time = PlaybackClock.GetTime() - ((long)PlaybackLog.Period * 1000);
        long duration = useLerp ? ((long)PlaybackLog.Period * 1000) : 0;
        recordLog.log.Add(PlaybackLogAction2.PlayBackActionFactory.CreateMovement(time, duration, subject, destination, rotation, scale, parent));
    }

    public static void LogEnable(GameObject subject)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.PlayBackActionFactory.CreateEnable(time, subject));
    }

    public static void LogDisable(GameObject subject)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.PlayBackActionFactory.CreateDisable(time, subject));
    }

    public static void LogDestroy(GameObject subject)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.PlayBackActionFactory.CreateDestroy(time, subject));
    }

    public static void LogButtonPress(GameObject subject, GameObject presser)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.PlayBackActionFactory.CreateButtonPress(time, subject, presser));
    }

    public static void LogButtonUnpress(GameObject subject, GameObject presser)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.PlayBackActionFactory.CreateButtonUnpress(time, subject, presser));
    }
}
