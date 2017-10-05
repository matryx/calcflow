using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;

public class PlaybackLog : Nanome.Core.Behaviour
{

    static void SaveLog()
    {
        string savePath = Path.Combine(Path.Combine(
            System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments),
            "Calcflow"), "Recordings");

        JsonUtility.ToJson(log);
        //System.IO.File.WriteAllText(Path.Combine(savePath, fileName) + jsonExtension, json);

    }

    [SerializeField]
    static Queue<PlayBackLogAction> log = new Queue<PlayBackLogAction>();

    public static void LogMovement(float timestamp, Vector3 destination)
    {
        log.Enqueue(PlayBackLogAction.CreateMovement(timestamp, destination));
    }

    public static void LogButtonPress(float timestamp, GameObject presser)
    {
        log.Enqueue(PlayBackLogAction.CreateButtonPress(timestamp, presser));
    }

    [Serializable]
    internal class PlayBackLogAction {

        public enum ActionType
        {
            Movement,
            ButtonPress
        }
        [SerializeField]
        ActionType type;
        [SerializeField]
        float timeStamp;
        [SerializeField]
        Vector3 dest;
        [SerializeField]
        GameObject ButtonPresser;

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

        internal static PlayBackLogAction CreateMovement(float timestamp, Vector3 destination)
        {
            PlayBackLogAction newAction = new PlayBackLogAction();
            newAction.type = ActionType.Movement;
            newAction.dest = destination;
            return newAction;
        }

        internal static PlayBackLogAction CreateButtonPress(float timestamp, GameObject presser)
        {
            PlayBackLogAction newAction = new PlayBackLogAction();
            newAction.type = ActionType.ButtonPress;
            newAction.ButtonPresser = presser;
            return newAction;
        }

    }
}
