using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Recorder : MonoBehaviour {

    public bool EditorRecord = false;
    public static PlaybackClock clock = new PlaybackClock();
    private static HashSet<int> AllGameObjects = new HashSet<int>();

    private static bool record = false;

    private void Start()
    {
    }

    private void Update()
    {
        Recording = EditorRecord;
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
                PlaybackLog.LogSpawn(gObj);
                gObj.AddComponent<MovementLogger>();
                AllGameObjects.Add(gObj.GetInstanceID());
            }
        }
    }
}
