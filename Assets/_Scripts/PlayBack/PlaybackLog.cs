using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;
using Nanome.Maths;
using Extensions;
using CalcFlowUI;

public class PlaybackLog : Nanome.Core.Behaviour
{
    [SerializeField]
    private void Start()
    {
    }

    public static PlaybackLog recordingInstance;
    public const float Period = .03f;

    [SerializeField]
    public float period = .03f;

    static string jsonExtension = "json";
    static string fileName = "recording1";

    private void Awake()
    {
        recordingInstance = this;
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

        System.IO.File.WriteAllText(Path.Combine(savePath, fileName) + jsonExtension, JsonUtility.ToJson(recordingInstance.log));

    }

    [SerializeField]
    List<PlayBackLogAction> log = new List<PlayBackLogAction>();

    public List<PlayBackLogAction> GetLogCopy()
    {
        return new List<PlayBackLogAction>(log);
    }

    public static void LogSpawn(GameObject subject)
    {
        recordingInstance.log.Add(PlayBackLogAction.CreateSpawn(PlaybackClock.GetTime(), subject, subject.transform.position, subject.transform.rotation, subject.transform.lossyScale));
    }

    public static void LogMovement(GameObject subject, Vector3 destination, Quaternion rotation, Vector3 scale)
    {
        recordingInstance.log.Add(PlayBackLogAction.CreateMovement(PlaybackClock.GetTime() - (long)(PlaybackLog.Period * 1000), subject, destination, rotation, scale));
    }

    public static void LogButtonPress(GameObject subject, GameObject presser)
    {
        recordingInstance.log.Add(PlayBackLogAction.CreateButtonPress(PlaybackClock.GetTime(), subject, presser));
    }

    public static void LogButtonUnpress(GameObject subject, GameObject presser)
    {
        recordingInstance.log.Add(PlayBackLogAction.CreateButtonUnpress(PlaybackClock.GetTime(), subject, presser));
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


        internal static PlayBackLogAction CreateSpawn(long timestamp, GameObject subject, Vector3 position, Quaternion rotation, Vector3 scale)
        {
            PlayBackLogAction newAction = new PlayBackLogAction
            {
                type = ActionType.Spawn,
                timeStamp = timestamp,
                position = position,
                rotation = rotation,
                scale = scale,
                subject = subject
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
                    GameObject spawned = Instantiate(subject);
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

}
