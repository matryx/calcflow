using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using Extensions;
using UnityEngine;

public class Recorder : MonoBehaviour {

    public bool EditorRecord = false;
    private static HashSet<int> AllGameObjects = new HashSet<int>();

    private static bool record = false;
    [SerializeField]
    public static PlaybackLog recordLog= new PlaybackLog();

    private void Start()
    {
    }

    [SerializeField]
    int[] testList;
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
                Transform child1 = gObj.transform;
                Transform parent1 = child1.parent;

                while(parent1 != null && !AllGameObjects.Contains(parent1.GetInstanceID()))
                {
                    child1 = parent1;
                    parent1 = child1.parent;
                }

                foreach (Transform descendent in child1.GetComponentsInChildren<Transform>())
                {
                    AllGameObjects.Add(descendent.gameObject.GetInstanceID());
                    descendent.gameObject.EnsureOneOf<MovementLogger>();
                }

                LogSpawn(child1.gameObject);
            }
        }
    }

    public static void LogSpawn(GameObject subject)
    {
        recordLog.log.Add(PlayBackLogAction.CreateSpawn(PlaybackClock.GetTime() - (long)(PlaybackLog.Period * 1000), 
                          JsonUtility.ToJson(subject), 
                          subject.transform.position, 
                          subject.transform.rotation, 
                          subject.transform.lossyScale));
    }

    public static void LogMovement(GameObject subject, Vector3 destination, Quaternion rotation, Vector3 scale)
    {
        recordLog.log.Add(PlayBackLogAction.CreateMovement(PlaybackClock.GetTime() - (long)(PlaybackLog.Period * 1000), 
                          subject, destination, rotation, scale));
    }

    public static void LogButtonPress(GameObject subject, GameObject presser)
    {
        recordLog.log.Add(PlayBackLogAction.CreateButtonPress(PlaybackClock.GetTime(), subject, presser));
    }

    public static void LogButtonUnpress(GameObject subject, GameObject presser)
    {
        recordLog.log.Add(PlayBackLogAction.CreateButtonUnpress(PlaybackClock.GetTime(), subject, presser));
    }
}
