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
    [SerializeField]
    private void Start()
    {
    }

    public const float Period = .03f;

    [SerializeField]
    public float period = .03f;

    static string jsonExtension = "json";
    static string fileName = "recording1";

    public PlaybackLog2()
    {
    }

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
        return log.ToList();
    }

    public class myReverserClass : IComparer
    {
        public int Compare(object x, object y)
        {
            PlaybackLogAction2 first = (PlaybackLogAction2)x;
            PlaybackLogAction2 second = (PlaybackLogAction2)y;

            if (first.timeStamp > second.timeStamp) return 1;
            if (first.timeStamp < second.timeStamp) return -1;
            return 0;
        }
    }

}

[Serializable]
public class PlaybackLogAction2
{
    internal static Dictionary<int, GameObject> objectMap = new Dictionary<int, GameObject>();

    public enum ActionType
    {
        Spawn,
        Destroy,
        Movement,
        ButtonPress,
        ButtonUnpress,
        Enable,
        Disable
    }
    [SerializeField]
    public int subjectKey;
    [SerializeField]
    public long timeStamp;
    [SerializeField]
    public ActionType type;

    [SerializeField]
    public LogInfo _info = new LogInfo();

    [HideInInspector]
    [SerializeField]
    internal string binaryRepresentation;




    internal void SetType(string type)
    {
        switch (type)
        {
            case "Movement":
                this.type = ActionType.Movement;
                break;
            case "ButtonPress":
                this.type = ActionType.ButtonPress;
                break;
            default:
                break;
        }
    }
    internal void SetType(ActionType type)
    {
        this.type = type;
    }

    //how many serializations are left before the scene is done recording.
    public static int numRunningSerializations;
    public static Queue<Action> spawnQueue = new Queue<Action>();
    public static IEnumerator spawner;

    public static IEnumerator steadySpawn()
    {
        while (spawnQueue.Count != 0)
        {

            spawnQueue.Dequeue().Invoke();
            yield return null;
        }
        spawner = null;
    }


    //Creates a spawn PlayBackLogAction. The action will not have an accurate binaryRepresentation until later.
    internal static PlaybackLogAction2 CreateSpawn(long timestamp, GameObject subject, Vector3 position, Quaternion rotation, Vector3 scale)
    {
        int key = subject.GetInstanceID();
        PlaybackLogAction2 newAction = new PlaybackLogAction2
        {
            type = ActionType.Spawn,
            timeStamp = timestamp,
            subjectKey = key
        };

        newAction._info.AddValue("position", position);
        newAction._info.AddValue("rotation", rotation);
        newAction._info.AddValue("scale", scale);
        Debug.Log("subjectkey: " + key + "\nposition: " + position + " rotation: " + rotation + " scale: " + scale);
        Debug.Log(newAction._info.MemberCount);
        position = newAction._info.GetValue<Vector3>("position");
        scale = newAction._info.GetValue<Vector3>("scale");
        rotation = newAction._info.GetValue<Quaternion>("rotation");
        Debug.Log("subjectkey: " + key + "\nposition: " + position + " rotation: " + rotation + " scale: " + scale);

        numRunningSerializations++;
        //enqueue a function that will perform the serialization of the data at a later time.
        spawnQueue.Enqueue(delegate ()
        {
            //Debug.Log(numRunningSerializations);
            numRunningSerializations--;
            newAction.SerializeForSpawn(subject, key.ToString());
        });
        if (spawner == null)
        {
            spawner = steadySpawn();
            Dispatcher.queue(spawner);
        }
        return newAction;
    }

    // function that will serialize the spawn and assign the binary output to "binaryRepresentation".
    void SerializeForSpawn(GameObject subject, string key)
    {
        binaryRepresentation = "";
        binaryRepresentation = RSManager.Serialize(subject, key);
    }

    internal static PlaybackLogAction2 CreateMovement(long timestamp, GameObject subject, Vector3 destination, Quaternion rotation, Vector3 scale, GameObject parent)
    {
        int parentKey = (parent == null) ? 0 : parent.GetInstanceID();
        int key = subject.GetInstanceID();
        PlaybackLogAction2 newAction = new PlaybackLogAction2
        {
            type = ActionType.Movement,
            timeStamp = timestamp,
            subjectKey = key,
        };

        newAction._info.AddValue("position", destination);
        newAction._info.AddValue("rotation", rotation);
        newAction._info.AddValue("scale", scale);
        newAction._info.AddValue("parentKey", parentKey);
        Debug.Log("subjectkey: " + key + "\nposition: " + destination + " rotation: " + rotation + " scale: " + scale + " parentkey " + parentKey);
        Debug.Log(newAction._info.MemberCount);
        Debug.Log(newAction._info.testEntry.Name);
        return newAction;
    }

    internal static PlaybackLogAction2 CreateEnable(long timestamp, GameObject subject)
    {
        PlaybackLogAction2 newAction = new PlaybackLogAction2
        {
            type = ActionType.Enable,
            timeStamp = timestamp,
            subjectKey = subject.GetInstanceID(),
        };
        return newAction;
    }

    internal static PlaybackLogAction2 CreateDisable(long timestamp, GameObject subject)
    {
        PlaybackLogAction2 newAction = new PlaybackLogAction2
        {
            type = ActionType.Disable,
            timeStamp = timestamp,
            subjectKey = subject.GetInstanceID(),
        };
        return newAction;
    }

    internal static PlaybackLogAction2 CreateDestroy(long timestamp, GameObject subject)
    {
        PlaybackLogAction2 newAction = new PlaybackLogAction2
        {
            type = ActionType.Destroy,
            timeStamp = timestamp,
            subjectKey = subject.GetInstanceID(),
        };
        return newAction;
    }

    //internal static PlaybackLogAction2 CreateButtonPress(long timestamp, GameObject subject, GameObject presser)
    //{
    //    PlaybackLogAction2 newAction = new PlaybackLogAction2
    //    {
    //        type = ActionType.ButtonPress,
    //        timeStamp = timestamp,
    //        buttonPresser = presser,
    //        subjectKey = subject.GetInstanceID()
    //    };
    //    return newAction;
    //}

    //internal static PlaybackLogAction2 CreateButtonUnpress(long timestamp, GameObject subject, GameObject presser)
    //{
    //    PlaybackLogAction2 newAction = new PlaybackLogAction2
    //    {
    //        type = ActionType.ButtonUnpress,
    //        timeStamp = timestamp,
    //        buttonPresser = presser,
    //        subjectKey = subject.GetInstanceID()
    //    };
    //    return newAction;
    //}

    void Spawn()
    {
        GameObject subject;
        subject = RSManager.DeserializeData<GameObject>(binaryRepresentation, subjectKey.ToString());
        //yield return null;
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
        Button button;
        GameObject subject;
        Vector3 position;
        Vector3 scale;
        Quaternion rotation;
        int parentKey;

        switch (type)
        {
            case ActionType.Spawn:
                //Dispatcher.queue(Spawn());
                Spawn();
                //Thread thread = new Thread(() => Spawn());

                //thread.Join();
                subject = objectMap[subjectKey];
                position = _info.GetValue<Vector3>("position");
                scale = _info.GetValue<Vector3>("scale");
                rotation = _info.GetValue<Quaternion>("rotation");

                Debug.Log("subjectkey: " + subjectKey + "\nposition: " + position + " rotation: " + rotation + " scale: " + scale);
                Debug.Log(_info.MemberCount);
                subject.MoveTo(position, 0);
                subject.RotateTo(rotation, 0);
                subject.GlobalScaleTo(scale, 0);
                break;
            case ActionType.Movement:
                if (objectMap.ContainsKey(subjectKey))
                {
                    subject = objectMap[subjectKey];
                    position = _info.GetValue<Vector3>("position");
                    scale = _info.GetValue<Vector3>("scale");
                    rotation = _info.GetValue<Quaternion>("rotation");
                    parentKey = _info.GetValue<int>("parentKey");
                    Debug.Log("subjectkey: " + subjectKey + "\nposition: " + position + " rotation: " + rotation + " scale: " + scale + " parentkey " + parentKey);
                    Debug.Log(_info.MemberCount);
                    Debug.Log(_info.testEntry.Name);

                    subject.LocalMoveTo(position, PlaybackLog.Period);
                    subject.RotateTo(rotation, PlaybackLog.Period);
                    subject.GlobalScaleTo(scale, PlaybackLog.Period);
                    subject.transform.parent = (parentKey == 0) ? null : objectMap[parentKey].transform;
                }
                else
                {
                    Debug.Log(timeStamp + " " + subjectKey);
                }
                break;
            case ActionType.Enable:
                if (objectMap.ContainsKey(subjectKey))
                {
                    subject = objectMap[subjectKey];
                    subject.SetActive(true);
                }
                else
                {
                    Debug.Log(timeStamp + " " + subjectKey);
                }
                break;
            case ActionType.Disable:
                if (objectMap.ContainsKey(subjectKey))
                {
                    subject = objectMap[subjectKey];
                    subject.SetActive(false);
                }
                else
                {
                    Debug.Log(timeStamp + " " + subjectKey);
                }
                break;
            case ActionType.Destroy:
                if (objectMap.ContainsKey(subjectKey))
                {
                    subject = objectMap[subjectKey];
                    UnityEngine.Object.Destroy(subject, 0);
                }
                else
                {
                    Debug.Log(timeStamp + " " + subjectKey);
                }
                break;
                //case ActionType.ButtonPress:
                //    subject = objectMap[subjectKey];
                //    button = subject.GetComponent<Button>();
                //    button.PressButton(this.buttonPresser);
                //    break;
                //case ActionType.ButtonUnpress:
                //    subject = objectMap[subjectKey];
                //    button = subject.GetComponent<Button>();
                //    button.UnpressButton(this.buttonPresser);
                //    break;
        }
    }



}

