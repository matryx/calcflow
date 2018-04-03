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

    public PlaybackLog2()
    {
        if (!PlaybackLogAction2.objectMap.ContainsKey(0))
            PlaybackLogAction2.objectMap.Add(0, null);
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
public class PlaybackLogAction2
{
    public static Dictionary<int, GameObject> objectMap = new Dictionary<int, GameObject>();

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
        newAction._info.AddValue("name", subject.name);
        newAction._info.AddValue("position", position);
        newAction._info.AddValue("rotation", rotation);
        newAction._info.AddValue("scale", scale);

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

    internal static PlaybackLogAction2 CreateButtonPress(long timestamp, GameObject subject, GameObject presser)
    {
        PlaybackLogAction2 newAction = new PlaybackLogAction2
        {
            type = ActionType.ButtonPress,
            timeStamp = timestamp,
            subjectKey = subject.GetInstanceID()
        };
        newAction._info.AddValue("buttonPresser", presser.GetInstanceID());
        return newAction;
    }

    internal static PlaybackLogAction2 CreateButtonUnpress(long timestamp, GameObject subject, GameObject presser)
    {
        PlaybackLogAction2 newAction = new PlaybackLogAction2
        {
            type = ActionType.ButtonUnpress,
            timeStamp = timestamp,
            subjectKey = subject.GetInstanceID()
        };
        newAction._info.AddValue("buttonPresser", presser);
        return newAction;
    }

    HashSet<int> brokenparents = new HashSet<int>();

    void Spawn()
    {
        GameObject subject;
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
        GameObject buttonPresser;
        int parentKey;

        switch (type)
        {
            case ActionType.Spawn:
                //Debug.Log("spawning");
                Spawn();
                subject = objectMap[subjectKey];
                position = _info.GetValue<Vector3>("position");
                scale = _info.GetValue<Vector3>("scale");
                rotation = _info.GetValue<Quaternion>("rotation");

                subject.MoveTo(position, 0);
                subject.RotateTo(rotation, 0);
                subject.GlobalScaleTo(scale, 0);
                break;
            case ActionType.Movement:
                subject = getObject(subjectKey);

                if (subject != null)
                {
                    position = _info.GetValue<Vector3>("position");
                    scale = _info.GetValue<Vector3>("scale");
                    rotation = _info.GetValue<Quaternion>("rotation");
                    parentKey = _info.GetValue<int>("parentKey");

                    if (objectMap.ContainsKey(parentKey))
                    {
                        if (brokenparents.Contains(subjectKey)) {
                            Debug.Log("successful parent");
                        }
                        subject.transform.SetParent((parentKey == 0) ? null : objectMap[parentKey].transform, false);
                    }
                    else
                    {
                        brokenparents.Add(subjectKey);
                        Debug.Log(timeStamp + " " + subject.name + " could not reparent because parent does not exist." );
                    }

                    subject.LocalMoveTo(position, PlaybackLog.Period);
                    subject.RotateTo(rotation, PlaybackLog.Period);
                    subject.GlobalScaleTo(scale, PlaybackLog.Period);
                    //if (!objectMap.ContainsKey(parentKey)){
                    //    Debug.Log("movement Action\n" +
                    //          "parentkey: " + parentKey + "\n" +
                    //          "ParentExists: " + false);
                    //}
                    //Debug.Log("movement Action\n" + 
                    //          "parentkey: " + parentKey + "\n" +
                    //          "ParentExists: " + true + "\n" +
                    //          "ParentName: " + ((objectMap[parentKey] != null) ? objectMap[parentKey].name : " N/A"));

                }
                else
                {
                    Debug.Log(timeStamp + " subject " + subjectKey + " could not be moved because subject does not exist.");
                }
                break;
            case ActionType.Enable:
                // if (objectMap.ContainsKey(subjectKey))
                // {
                //     subject = objectMap[subjectKey];
                //     subject.SetActive(true);
                // }
                // else
                // {
                //     Debug.Log(timeStamp + " " + subjectKey);
                // }
                break;
            case ActionType.Disable:
                subject = getObject(subjectKey);

                if (subject != null)
                {
                    subject.SetActive(false);
                }
                else
                {
                    Debug.Log(timeStamp + " " + subjectKey);
                }
                break;
            case ActionType.Destroy:
                subject = getObject(subjectKey);

                if (subject != null)
                {
                    UnityEngine.Object.Destroy(subject, 0);
                }
                else
                {
                    Debug.Log(timeStamp + " " + subjectKey);
                }
                break;
            case ActionType.ButtonPress:
                subject = getObject(subjectKey);

                if (subject != null)
                {
                    button = subject.GetComponent<Button>();
                    buttonPresser = getObject(_info.GetValue<int>("buttonPresser"));
                    button.PressButton(buttonPresser);
                }
                else
                {
                    Debug.Log(timeStamp + " " + subjectKey);
                }
                break;
            case ActionType.ButtonUnpress:
                subject = getObject(subjectKey);

                if (subject != null)
                {
                    button = subject.GetComponent<Button>();

                    buttonPresser = getObject(_info.GetValue<int>("buttonPresser"));

                    button.UnpressButton(buttonPresser);
                }
                else
                {
                    Debug.Log(timeStamp + " " + subjectKey);
                }
                break;
        }
    }



    GameObject getObject(int ID)
    {
        if (objectMap.ContainsKey(ID))
        {
            return objectMap[ID];
        }
        else
        {
            return null;
        }
    }

}

