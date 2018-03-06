using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using Extensions;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;
using Extensions;
using CalcFlowUI;


public class Recorder : MonoBehaviour
{

    public bool EditorRecord = false;
    private static HashSet<int> AllGameObjects = new HashSet<int>();
    private static List<UIDSystem> AllUIDs = new List<UIDSystem>();
    private static bool record = false;
    [SerializeField]
    public static PlaybackLog2 recordLog = new PlaybackLog2();

    private void Start()
    {
        //AllGameObjects.Add(gameObject.GetInstanceID());
        AllUIDs.Remove(gameObject.GetComponent<UIDSystem>());
    }

    private void Update()
    {
        Recording = EditorRecord;
    }

    private static void StartRecording()
    {
        print("start recording");
        PlaybackClock.StartClock();
        foreach(UIDSystem uid in AllUIDs){
            RecordSpawn(uid);
        }
        //PlaybackClock.AddToTimer(CheckForSpawns);
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
        AllUIDs.Add(uid);
        if (Recording){
            RecordSpawn(uid);
        }
    }

    private static void RecordSpawn(UIDSystem uid){
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
        foreach (GameObject gObj in (GameObject[])GameObject.FindObjectsOfType<GameObject>())
        {
            // if (!AllGameObjects.Contains(gObj.GetInstanceID()))
            // {
            //     AllGameObjects.Add(gObj.GetInstanceID());

            //     if (gObj.GetComponent<UIDSystem>())
            //     {
            //         LogSpawn(gObj);
            //         if (gObj.GetComponent<Button>() != null)
            //         {
            //             gObj.EnsureOneOf<ButtonLogger>();
            //         }
            //         gObj.EnsureOneOf<EnableLogger>();
            //         gObj.EnsureOneOf<MovementLogger>();
            //     }
                //Transform child1 = gObj.transform;
                //Transform parent1 = child1.parent;

                //while (parent1 != null && !AllGameObjects.Contains(parent1.GetInstanceID()))
                //{
                //    child1 = parent1;
                //    parent1 = child1.parent;
                //}

                //foreach (Transform descendent in child1.GetComponentsInChildren<Transform>())
                //{
                //    if (child1.gameObject.GetComponent<UIDSystem>())
                //    {
                //        AllGameObjects.Add(descendent.gameObject.GetInstanceID());
                //        descendent.gameObject.EnsureOneOf<EnableLogger>();
                //        descendent.gameObject.EnsureOneOf<MovementLogger>();
                //        LogSpawn(child1.gameObject);
                //    }
                //}
                //if(child1.gameObject.GetComponent<UIDSystem>())
                //    LogSpawn(child1.gameObject);
            //}
        }
    }

    public static void LogSpawn(GameObject subject)
    {
        long time = PlaybackClock.GetTime() - (long)(PlaybackLog2.Period * 1001);
        recordLog.log.Add(PlaybackLogAction2.CreateSpawn(time,
                                                            subject,
                                                            subject.transform.position,
                                                            subject.transform.rotation,
                                                            subject.transform.lossyScale));
    }

    public static void LogMovement(GameObject subject, Vector3 destination, Quaternion rotation, Vector3 scale, GameObject parent)
    {
        long time = PlaybackClock.GetTime() - (long)(PlaybackLog.Period * 1000);
        recordLog.log.Add(PlaybackLogAction2.CreateMovement(time,
                          subject, destination, rotation, scale, parent));
    }

    public static void LogEnable(GameObject subject)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.CreateEnable(time, subject));
    }

    public static void LogDisable(GameObject subject)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.CreateDisable(time, subject));
    }

    public static void LogDestroy(GameObject subject)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.CreateDestroy(time, subject));
    }

    public static void LogButtonPress(GameObject subject, GameObject presser)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.CreateButtonPress(time, subject, presser));
    }

    public static void LogButtonUnpress(GameObject subject, GameObject presser)
    {
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.CreateButtonUnpress(time, subject, presser));
    }
}
