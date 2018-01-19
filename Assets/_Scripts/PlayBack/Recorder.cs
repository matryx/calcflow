using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using Extensions;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;


public class Recorder : MonoBehaviour {

    public bool EditorRecord = false;
    private static HashSet<int> AllGameObjects = new HashSet<int>();

    private static bool record = false;
    [SerializeField]
    public static PlaybackLog2 recordLog= new PlaybackLog2();

    private void Start()
    {
        AllGameObjects.Add(gameObject.GetInstanceID());
    }

    private void Update()
    {
        Recording = EditorRecord;
        //testList = new int[AllGameObjects.Count];
        //AllGameObjects.CopyTo(testList);
        //foreach(int i in ddddtestList)
        //{
        //    print(EditorUtility.InstanceIDToObject(i).name);
        //}
    }

    private static void StartRecording()
    {
        PlaybackClock.StartClock();
        PlaybackClock.AddToTimer(CheckForSpawns);
    }

    private static void StopRecording()
    {
        PlaybackClock.StopClock();
    }

    public static bool Recording
    {
        get
        {
            return record;
        } set
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

    private static void CheckForSpawns()
    {
        foreach (GameObject gObj in (GameObject[])GameObject.FindObjectsOfType<GameObject>())
        {
            if (!AllGameObjects.Contains(gObj.GetInstanceID())){
                AllGameObjects.Add(gObj.GetInstanceID());

                if (gObj.GetComponent<UIDSystem>())
                {
                    gObj.EnsureOneOf<EnableLogger>();
                    gObj.EnsureOneOf<MovementLogger>();
                    LogSpawn(gObj);
                }
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
            }
        }
    }

    public static void LogSpawn(GameObject subject)
    {
        long time = PlaybackClock.GetTime() - (long)(PlaybackLog2.Period * 1000);
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

    public static void LogEnable(GameObject subject){
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.CreateEnable(time, subject));
    }

    public static void LogDisable(GameObject subject){
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.CreateDisable(time, subject));
    }

    public static void LogDestroy(GameObject subject){
        long time = PlaybackClock.GetTime();
        recordLog.log.Add(PlaybackLogAction2.CreateDestroy(time, subject));
    }

    public static void LogButtonPress(GameObject subject, GameObject presser)
    {
        long time = PlaybackClock.GetTime();
        //recordLog.log.Add(PlaybackLogAction2.CreateButtonPress(time, subject, presser));
    }

    public static void LogButtonUnpress(GameObject subject, GameObject presser)
    {
        long time = PlaybackClock.GetTime();
        //recordLog.log.Add(PlaybackLogAction2.CreateButtonUnpress(time, subject, presser));
    }
}
