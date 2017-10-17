using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;
using Nanome.Maths;
using Extensions;

public class PlaybackLog : Nanome.Core.Behaviour
{

    private void Awake()
    {
        instance = this;
    }

    public static PlaybackLog instance;
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

        System.IO.File.WriteAllText(Path.Combine(savePath, fileName) + jsonExtension, JsonUtility.ToJson(instance.log));

    }

    [SerializeField]
    List<PlayBackLogAction> log = new List<PlayBackLogAction>();

    public List<PlayBackLogAction> GetLogCopy()
    {
        return new List<PlayBackLogAction>(log);
    }

    public static void LogMovement(long timestamp, GameObject subject, Vector3 destination)
    {
        instance.log.Add(PlayBackLogAction.CreateMovement(timestamp, subject, destination));
    }

    public static void LogButtonPress(long timestamp, GameObject subject, GameObject presser)
    {
        instance.log.Add(PlayBackLogAction.CreateButtonPress(timestamp, subject, presser));
    }

    [Serializable]
    public class PlayBackLogAction
    {

        public enum ActionType
        {
            Movement,
            ButtonPress
        }
        [SerializeField]
        public ActionType type;
        [SerializeField]
        public GameObject subject;
        [SerializeField]
        public long timeStamp;
        [SerializeField]
        public Vector3 dest;
        [SerializeField]
        public GameObject ButtonPresser;

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

        PlayBackLogAction()
        {

        }

        internal static PlayBackLogAction CreateMovement(long timestamp, GameObject subject, Vector3 destination)
        {
            PlayBackLogAction newAction = new PlayBackLogAction
            {
                type = ActionType.Movement,
                timeStamp = timestamp,
                dest = destination,
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
                ButtonPresser = presser,
                subject = subject
            };
            return newAction;
        }

        public void Reenact()
        {
            switch (type)
            {
                case ActionType.Movement:
                    print("moving to: " + dest);
                    subject.MoveTo(dest, PlaybackLog.Period);
                    break;
                case ActionType.ButtonPress:
                    break;
                default:
                    break;
            }
        }
    }
}
