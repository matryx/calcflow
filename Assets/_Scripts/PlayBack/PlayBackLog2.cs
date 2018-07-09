using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;
using Nanome.Maths;
using Extensions;
using CalcFlowUI;
using VoxelBusters.RuntimeSerialization;
using Nanome.Core.Daemon;
using Priority_Queue;
using System.Threading;
using Nanome.Core;


[Serializable]
public class PlaybackLog2
{
    public const float Period = .03f;

    [SerializeField]
    public float period = .03f;

    static string jsonExtension = "json";
    static string fileName = "recording1";

    public static void SaveLog()
    {
        string savePath = Path.Combine(Path.Combine(
            System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments),
            "Calcflow"), "Recordings");

        if (!System.IO.Directory.Exists(savePath))
        {
            System.IO.Directory.CreateDirectory(savePath);
        }

        //System.IO.File.WriteAllText(Path.Combine(savePath, fileName) + jsonExtension, JsonUtility.ToJson(recordingInstance.log));

    }

    [SerializeField]
    public List<PlaybackLogAction2> log = new List<PlaybackLogAction2>();
    public List<PlaybackLogAction2> GetLogCopy()
    {
        //This sort removes a bug caused by the way movement is logged. 
        //Movement's time is logged as 1 timer period earlier than it actually happened to improve the accuracy of the lerp.
        //This doesn't work if it gets stuck behind something that isn't logged early since the recorder goes in list order.
        log.Sort(new myReverserClass());
        return log.ToList();
    }

    public class myReverserClass : IComparer<PlaybackLogAction2>
    {
        public int Compare(PlaybackLogAction2 x, PlaybackLogAction2 y)
        {
            if (x.timeStamp > y.timeStamp) return 1;
            if (x.timeStamp < y.timeStamp) return -1;
            return 0;
        }
    }

    public void Add(PlaybackLogAction2 action)
    {
        log.Add(action);
    }

}

[Serializable]
public partial class PlaybackLogAction2
{
    #region other stuff
    private static Dictionary<int, GameObject> objectMap = new Dictionary<int, GameObject>() { { 0, null } };

    public delegate void ReenactAction(LogInfo info, GameObject subject, PlaybackLogAction2 entry);
    protected static Dictionary<string, ReenactAction> Reenactors = new Dictionary<string, ReenactAction>();
    public static void RegisterReenactor(string key, ReenactAction ra)
    {
        if (!Reenactors.ContainsKey(key))
        {
            Reenactors.Add(key, ra);
        }
        else
        {
            Debug.Log("Reenactors already contains key: " + key);
        }
    }

    [SerializeField]
    public int subjectKey;
    [SerializeField]
    public long timeStamp;

    [SerializeField]
    public LogInfo _info = new LogInfo();

    [HideInInspector]
    [SerializeField]
    internal string binaryRepresentation;

    //how many serializations are left before the scene is done recording.
    public static int numRunningSerializations;
    #endregion
    public static Queue<Action> spawnQueue = new Queue<Action>();

    public static IEnumerator spawner;
    public static int spawnsPerFrame = 10;

    public static IEnumerator steadySpawn()
    {
        while (spawnQueue.Count != 0)
        {
            for (int i = 0; i < spawnsPerFrame; i++)
            {
                if (spawnQueue.Count == 0) break;
                spawnQueue.Dequeue().Invoke();
            }
            yield return null;
        }
        spawner = null;
    }

    // function that will serialize the spawn and assign the binary output to "binaryRepresentation".
    public void SerializeForSpawn(GameObject subject, string key)
    {
        binaryRepresentation = "";
        binaryRepresentation = RSManager.Serialize(subject, key);
    }

    void Spawn()
    {
        GameObject subject;
        //subject = RSManager.DeserializeData<GameObject>(binaryRepresentation, subjectKey.ToString());

        try
        {
            subject = RSManager.DeserializeData<GameObject>(binaryRepresentation, subjectKey.ToString());
        }
        catch (Exception e)
        {
            Debug.Log("Exception found in gameobject: " + _info.GetValue<string>("name") + " with subject key " + subjectKey);
            Debug.LogError(e.Message);
            throw e;
        }

        if (objectMap.ContainsKey(subjectKey))
        {
            objectMap[subjectKey] = subject;
        }
        else
        {
            objectMap.Add(subjectKey, subject);
        }

    }


    public void Reenact()
    {
        GameObject subject;

        string key = _info.GetValue<string>("key");

        switch (key)
        {
            case "spawn":
                subject = GetObject(subjectKey);
                ReenactSpawn(_info, subject, this);
                break;
            case "enable":
                subject = GetObject(subjectKey);
                ReenactEnable(_info, subject, this);
                break;
            case "disable":
                subject = GetObject(subjectKey);
                ReenactEnable(_info, subject, this);
                break;
            case "destroy":
                subject = GetObject(subjectKey);
                ReenactDestroy(_info, subject, this);
                break;
            default:
                subject = GetObject(subjectKey);
                ReenactAction reenactor;
                if (Reenactors.TryGetValue(key, out reenactor))
                {
                    reenactor(_info, subject, this);
                }
                else
                {
                    Debug.LogError("Could not find reenactor for key " + key);
                }
                break;
        }
    }

    //Basic Reenactors
    #region basic reenactors
    private void ReenactSpawn(LogInfo _info, GameObject subject, PlaybackLogAction2 entry)
    {
        Vector3 position;
        Vector3 scale;
        Quaternion rotation;
        long duration;

        Spawn();
        subject = objectMap[subjectKey];
        position = _info.GetValue<Vector3>("position");
        scale = _info.GetValue<Vector3>("scale");
        rotation = _info.GetValue<Quaternion>("rotation");

        // if (subject.name == "PieceWiseTabs")
        // {
        //     Debug.Log("delete parent is being made. key: " + subjectKey);
        // }
        subject.MoveTo(position, 0);
        subject.RotateTo(rotation, 0);
        subject.GlobalScaleTo(scale, 0);
    }

    private void ReenactEnable(LogInfo _info, GameObject subject, PlaybackLogAction2 entry)
    {
        int parentKey;
        Vector3 position;
        Vector3 scale;
        Quaternion rotation;
        long duration;

        // if (objectMap.ContainsKey(subjectKey))
        // {
        //     subject = objectMap[subjectKey];
        //     subject.SetActive(true);
        // }
        // else
        // {
        //     Debug.Log(timeStamp + " " + subjectKey);
        // }
    }

    private void ReenactDisable(LogInfo _info, GameObject subject, PlaybackLogAction2 entry)
    {
        int parentKey;
        Vector3 position;
        Vector3 scale;
        Quaternion rotation;
        long duration;

        subject = GetObject(subjectKey);

        if (subject != null)
        {
            subject.SetActive(false);
        }
        else
        {
            Debug.Log(timeStamp + " " + subjectKey);
        }

    }

    private void ReenactDestroy(LogInfo _info, GameObject subject, PlaybackLogAction2 entry)
    {
        subject = GetObject(subjectKey);

        if (subject != null)
        {
            UnityEngine.Object.Destroy(subject, 0);
        }
        else
        {
            Debug.Log(timeStamp + " " + subjectKey);
        }
    }


    #endregion
    public static GameObject GetObject(int ID)
    {
        GameObject outObject;
        TryGetObject(ID, out outObject);
        return outObject;
    }

    public static bool TryGetObject(int ID, out GameObject outObject)
    {
        if (objectMap.ContainsKey(ID))
        {
            outObject = objectMap[ID];
            return true;
        }
        else
        {
            outObject = null;
            return false;
        }
    }

}

