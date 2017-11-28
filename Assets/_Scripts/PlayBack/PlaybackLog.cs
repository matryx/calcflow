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
public class PlaybackLog
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

    public PlaybackLog()
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
    public List<PlayBackLogAction> log = new List<PlayBackLogAction>();

    public List<PlayBackLogAction> GetLogCopy()
    {
        return log.ToList();
    }

    public class myReverserClass : IComparer
    {
        public int Compare(object x, object y)
        {
            PlayBackLogAction first = (PlayBackLogAction)x;
            PlayBackLogAction second = (PlayBackLogAction)y;

            if (first.timeStamp > second.timeStamp) return 1;
            if (first.timeStamp < second.timeStamp) return -1;
            return 0;
        }
    }

}
[Serializable]
public class PlayBackLogAction
{
    internal static Dictionary<int, GameObject> objectMap = new Dictionary<int, GameObject>();

    public enum ActionType
    {
        Spawn,
        Movement,
        ButtonPress,
        ButtonUnpress
    }
    [SerializeField]
    public int subjectKey;
    [SerializeField]
    public long timeStamp;
    [SerializeField]
    internal Vector3 position;
    [SerializeField]
    internal Quaternion rotation;
    [SerializeField]
    internal Vector3 scale;
    [SerializeField]
    internal GameObject buttonPresser;
    [SerializeField]
    [HideInInspector]
    internal string binaryRepresentation;


    [SerializeField]
    public ActionType type;

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
    internal static PlayBackLogAction CreateSpawn(long timestamp, GameObject subject, Vector3 position, Quaternion rotation, Vector3 scale)
    {
        int key = subject.GetInstanceID();
        PlayBackLogAction newAction = new PlayBackLogAction
        {
            type = ActionType.Spawn,
            timeStamp = timestamp,
            position = position,
            rotation = rotation,
            scale = scale,
            subjectKey = key
        };

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

    internal static PlayBackLogAction CreateMovement(long timestamp, GameObject subject, Vector3 destination, Quaternion rotation, Vector3 scale)
    {
        PlayBackLogAction newAction = new PlayBackLogAction
        {
            type = ActionType.Movement,
            timeStamp = timestamp,
            position = destination,
            rotation = rotation,
            scale = scale,
            subjectKey = subject.GetInstanceID()
        };

        return newAction;
    }

    internal static PlayBackLogAction CreateButtonPress(long timestamp, GameObject subject, GameObject presser)
    {
        PlayBackLogAction newAction = new PlayBackLogAction
        {
            type = ActionType.ButtonPress,
            timeStamp = timestamp,
            buttonPresser = presser,
            subjectKey = subject.GetInstanceID()
        };
        return newAction;
    }

    internal static PlayBackLogAction CreateButtonUnpress(long timestamp, GameObject subject, GameObject presser)
    {
        PlayBackLogAction newAction = new PlayBackLogAction
        {
            type = ActionType.ButtonUnpress,
            timeStamp = timestamp,
            buttonPresser = presser,
            subjectKey = subject.GetInstanceID()
        };
        return newAction;
    }

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
        switch (type)
        {
            case ActionType.Spawn:
                //Dispatcher.queue(Spawn());
                Spawn();
                //Thread thread = new Thread(() => Spawn());

                //thread.Join();
                subject = objectMap[subjectKey];
                subject.MoveTo(position, 0);
                subject.RotateTo(rotation, 0);
                subject.GlobalScaleTo(scale, 0);
                break;
            case ActionType.Movement:
                if (objectMap.ContainsKey(subjectKey))
                {
                    subject = objectMap[subjectKey];
                    subject.LocalMoveTo(position, PlaybackLog.Period);
                    subject.RotateTo(rotation, PlaybackLog.Period);
                    subject.GlobalScaleTo(scale, PlaybackLog.Period);
                }
                else
                {
                    Debug.Log(timeStamp + " " + subjectKey);
                }
                break;
            case ActionType.ButtonPress:
                subject = objectMap[subjectKey];
                button = subject.GetComponent<Button>();
                button.PressButton(this.buttonPresser);
                break;
            case ActionType.ButtonUnpress:
                subject = objectMap[subjectKey];
                button = subject.GetComponent<Button>();
                button.UnpressButton(this.buttonPresser);
                break;
        }
    }



}

