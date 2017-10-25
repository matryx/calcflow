using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;
using Nanome.Maths;
using Extensions;
using CalcFlowUI;

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
        return new List<PlayBackLogAction>(log);
    }

}
[Serializable]
public class PlayBackLogAction
{
    public enum ActionType
    {
        Spawn,
        Movement,
        ButtonPress,
        ButtonUnpress
    }
    [SerializeField]
    public GameObject subject;
    [SerializeField]
    public string serializedSubject;
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


    internal static PlayBackLogAction CreateSpawn(long timestamp, string serializedSubject , Vector3 position, Quaternion rotation, Vector3 scale)
    {
        PlayBackLogAction newAction = new PlayBackLogAction
        {
            type = ActionType.Spawn,
            timeStamp = timestamp,
            position = position,
            rotation = rotation,
            scale = scale,
            serializedSubject = serializedSubject
        };

        return newAction;
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
            subject = subject
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
            subject = subject,
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
            subject = subject,
        };
        return newAction;
    }

    public void Reenact()
    {
        Button button;
        switch (type)
        {
            case ActionType.Movement:
                subject.MoveTo(position, PlaybackLog.Period);
                subject.RotateTo(rotation, PlaybackLog.Period);
                subject.GlobalScaleTo(scale, PlaybackLog.Period);
                break;
            case ActionType.Spawn:
                
                GameObject spawned = GameObject.Instantiate(JsonUtility.FromJson<GameObject>(serializedSubject));
                spawned.transform.position = position;
                spawned.transform.rotation = rotation;
                spawned.transform.localScale = scale;
                break;
            case ActionType.ButtonPress:
                button = subject.GetComponent<Button>();
                button.PressButton(this.buttonPresser);
                break;
            case ActionType.ButtonUnpress:
                button = subject.GetComponent<Button>();
                button.UnpressButton(this.buttonPresser);
                break;
        }
    }
}

