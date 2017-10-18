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

    public static PlaybackLog recordingInstance;
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

        System.IO.File.WriteAllText(Path.Combine(savePath, fileName) + jsonExtension, JsonUtility.ToJson(recordingInstance.log));

    }

    [SerializeField]
    List<PlayBackLogAction> log = new List<PlayBackLogAction>();

    public List<PlayBackLogAction> GetLogCopy()
    {
        return new List<PlayBackLogAction>(log);
    }

    public static void LogMovement(long timestamp, GameObject subject, Vector3 destination)
    {
        recordingInstance.log.Add(PlayBackLogAction.CreateMovement(timestamp, subject, destination));
    }

    public static void LogButtonPress(long timestamp, GameObject subject, GameObject presser)
    {
        recordingInstance.log.Add(PlayBackLogAction.CreateButtonPress(timestamp, subject, presser));
    }

    [Serializable]
    public abstract class PlayBackLogAction
    {
        public enum ActionType
        {
            Movement,
            Rotation,
            ButtonPress,
            ButtonUnpress
        }
        [SerializeField]
        public GameObject subject;
        [SerializeField]
        public long timeStamp;

        ActionType type;

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

        internal PlayBackLogAction()
        {

        }

        internal static PlayBackLogAction CreateSpawn(long timestamp, GameObject subject, Vector3 position, Quaternion rotation, Vector3 scale)
        {
            PlayBackLogAction newAction = new SpawnLogAction
            {
                type = ActionType.Movement,
                timeStamp = timestamp,
                position = position,
                rotation = rotation,
                scale = scale,
                subject = subject
            };

            return newAction;
        }

        internal static PlayBackLogAction CreateMovement(long timestamp, GameObject subject, Vector3 destination)
        {
            PlayBackLogAction newAction = new MovementLogAction
            {
                type = ActionType.Movement,
                timeStamp = timestamp,
                dest = destination,
                subject = subject
            };

            return newAction;
        }

        internal static PlayBackLogAction CreateRotation(long timestamp, GameObject subject, Quaternion destination)
        {
            PlayBackLogAction newAction = new RotationLogAction
            {
                type = ActionType.Movement,
                timeStamp = timestamp,
                dest = destination,
                subject = subject
            };

            return newAction;
        }

        internal static PlayBackLogAction CreateScale(long timestamp, GameObject subject, Vector3 scale)
        {
            PlayBackLogAction newAction = new ScaleLogAction
            {
                type = ActionType.Movement,
                timeStamp = timestamp,
                scale = scale,
                subject = subject
            };

            return newAction;
        }

        internal static PlayBackLogAction CreateButtonPress(long timestamp, GameObject subject, GameObject presser)
        {
            PlayBackLogAction newAction = new ButtonPressLogAction
            {
                type = ActionType.ButtonPress,
                timeStamp = timestamp,
                ButtonPresser = presser,
                subject = subject,
            };
            return newAction;
        }

        internal static PlayBackLogAction CreateButtonUnpress(long timestamp, GameObject subject, GameObject presser)
        {
            PlayBackLogAction newAction = new ButtonUnpressLogAction
            {
                type = ActionType.ButtonUnpress,
                timeStamp = timestamp,
                ButtonPresser = presser,
                subject = subject,
            };
            return newAction;
        }

        public abstract void Reenact();
    }

    public class SpawnLogAction : PlayBackLogAction
    {
        [SerializeField]
        internal Vector3 position;
        [SerializeField]
        internal Quaternion rotation;
        [SerializeField]
        internal Vector3 scale;
    }

    public class MovementLogAction : PlayBackLogAction
    {
        [SerializeField]
        public Vector3 dest;
    }

    public class RotationLogAction : PlayBackLogAction
    {
        [SerializeField]
        public Quaternion dest;
    }

    public class ScaleLogAction : PlayBackLogAction
    {
        [SerializeField]
        public Vector3 scale;
    }

    public class ButtonPressLogAction : PlayBackLogAction
    {
        [SerializeField]
        public GameObject ButtonPresser;
    }

    public class ButtonUnpressLogAction : PlayBackLogAction
    {
        [SerializeField]
        public GameObject ButtonPresser;
    }
}
