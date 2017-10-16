using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;

public class PlaybackLog : Nanome.Core.Behaviour
{

    private void Awake()
    {
        Log = log;
    }

    public static List<PlayBackLogAction> Log;

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

        System.IO.File.WriteAllText(Path.Combine(savePath, fileName) + jsonExtension, JsonUtility.ToJson(Log));

    }

    [SerializeField]
    List<PlayBackLogAction> log = new List<PlayBackLogAction>();

    public static void LogMovement(float timestamp, GameObject subject, Vector3 destination)
    {
        Log.Add(PlayBackLogAction.CreateMovement(timestamp, subject, destination));
    }

    public static void LogButtonPress(float timestamp, GameObject subject, GameObject presser)
    {
        Log.Add(PlayBackLogAction.CreateButtonPress(timestamp, subject, presser));
    }

    [Serializable]
    public class PlayBackLogAction {

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
        public float timeStamp;
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

        internal static PlayBackLogAction CreateMovement(float timestamp, GameObject subject, Vector3 destination)
        {
            PlayBackLogAction newAction = new PlayBackLogAction();
            newAction.type = ActionType.Movement;
            newAction.dest = destination;
            newAction.subject = subject;
            return newAction;
        }

        internal static PlayBackLogAction CreateButtonPress(float timestamp, GameObject subject, GameObject presser)
        {
            PlayBackLogAction newAction = new PlayBackLogAction();
            newAction.type = ActionType.ButtonPress;
            newAction.ButtonPresser = presser;
            newAction.subject = subject;
            return newAction;
        }

        public void Reenact()
        {
            print("stuff");
        }
    }
}
