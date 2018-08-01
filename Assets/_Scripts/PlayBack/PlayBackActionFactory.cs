using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Nanome.Core;
using Nanome.Core.Daemon;
using Extensions;
using VoxelBusters.RuntimeSerialization;

public partial class PlaybackLogEntry
{
    public class PlayBackActionFactory
    {

        #region other constructors
        public static PlaybackLogEntry CreateMovement(long timestamp, long duration, GameObject subject, Vector3 destination, Quaternion rotation, Vector3 scale, GameObject parent)
        {
            string parentKey = (parent == null) ? "" : PlaybackLogEntry.GetUniqueID(parent);
            string key = PlaybackLogEntry.GetUniqueID(subject);
            PlaybackLogEntry newAction = new PlaybackLogEntry
            {
                timeStamp = timestamp,
                subjectKey = key,
            };
            newAction._info.AddValue("key", "movement");
            newAction._info.AddValue("duration", duration);
            newAction._info.AddValue("position", destination);
            newAction._info.AddValue("rotation", rotation);
            newAction._info.AddValue("scale", scale);
            newAction._info.AddValue("parentKey", parentKey);
            return newAction;
        }

        public static PlaybackLogEntry CreateEnable(long timestamp, GameObject subject)
        {
            PlaybackLogEntry newAction = new PlaybackLogEntry
            {
                timeStamp = timestamp,
                subjectKey = PlaybackLogEntry.GetUniqueID(subject),
            };
            newAction._info.AddValue("key", "enable");

            return newAction;
        }

        public static PlaybackLogEntry CreateDisable(long timestamp, GameObject subject)
        {
            PlaybackLogEntry newAction = new PlaybackLogEntry
            {
                timeStamp = timestamp,
                subjectKey = PlaybackLogEntry.GetUniqueID(subject),
            };
            newAction._info.AddValue("key", "disable");

            return newAction;
        }

        public static PlaybackLogEntry CreateDestroy(long timestamp, GameObject subject)
        {
            PlaybackLogEntry newAction = new PlaybackLogEntry
            {
                timeStamp = timestamp,
                subjectKey = PlaybackLogEntry.GetUniqueID(subject),
            };
            newAction._info.AddValue("key", "destroy");

            return newAction;
        }

        public static PlaybackLogEntry CreateButtonPress(long timestamp, GameObject subject, GameObject presser)
        {
            PlaybackLogEntry newAction = new PlaybackLogEntry
            {
                timeStamp = timestamp,
                subjectKey = PlaybackLogEntry.GetUniqueID(subject)
            };
            newAction._info.AddValue("key", "buttonPress");
            string presserID = (presser != null) ? GetUniqueID(presser) : null;
            newAction._info.AddValue("buttonPresser", presserID);
            return newAction;
        }

        public static PlaybackLogEntry CreateButtonUnpress(long timestamp, GameObject subject, GameObject presser)
        {
            PlaybackLogEntry newAction = new PlaybackLogEntry
            {
                timeStamp = timestamp,
                subjectKey = PlaybackLogEntry.GetUniqueID(subject)
            };
            newAction._info.AddValue("key", "buttonUnpress");
            string presserID = (presser != null) ? GetUniqueID(presser) : null;
            newAction._info.AddValue("buttonPresser", presserID);
            return newAction;
        }
        #endregion
        public static PlaybackLogEntry CreateSpawn(long timestamp, GameObject subject, Vector3 position, Quaternion rotation, Vector3 scale)
        {
            string key = PlaybackLogEntry.GetUniqueID(subject);
            PlaybackLogEntry newAction = new PlaybackLogEntry
            {
                timeStamp = timestamp,
                subjectKey = key
            };
            newAction._info.AddValue("key", "spawn");
            newAction._info.AddValue("name", subject.name);
            newAction._info.AddValue("position", position);
            newAction._info.AddValue("rotation", rotation);
            newAction._info.AddValue("scale", scale);

            PlaybackLogEntry.numRunningSerializations++;
            newAction.SerializeForSpawn(subject, key);
            return newAction;
        }
    }
}