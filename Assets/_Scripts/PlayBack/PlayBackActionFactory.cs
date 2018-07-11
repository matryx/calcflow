using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Nanome.Core;
using Nanome.Core.Daemon;
using Extensions;

public partial class PlaybackLogEntry
{
    public class PlayBackActionFactory
    {

        #region other constructors
        public static PlaybackLogEntry CreateMovement(long timestamp, long duration, GameObject subject, Vector3 destination, Quaternion rotation, Vector3 scale, GameObject parent)
        {
            int parentKey = (parent == null) ? 0 : parent.GetInstanceID();
            int key = subject.GetInstanceID();
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
                subjectKey = subject.GetInstanceID(),
            };
            newAction._info.AddValue("key", "enable");

            return newAction;
        }

        public static PlaybackLogEntry CreateDisable(long timestamp, GameObject subject)
        {
            PlaybackLogEntry newAction = new PlaybackLogEntry
            {
                timeStamp = timestamp,
                subjectKey = subject.GetInstanceID(),
            };
            newAction._info.AddValue("key", "disable");

            return newAction;
        }

        public static PlaybackLogEntry CreateDestroy(long timestamp, GameObject subject)
        {
            PlaybackLogEntry newAction = new PlaybackLogEntry
            {
                timeStamp = timestamp,
                subjectKey = subject.GetInstanceID(),
            };
            newAction._info.AddValue("key", "destroy");

            return newAction;
        }

        public static PlaybackLogEntry CreateButtonPress(long timestamp, GameObject subject, GameObject presser)
        {
            PlaybackLogEntry newAction = new PlaybackLogEntry
            {
                timeStamp = timestamp,
                subjectKey = subject.GetInstanceID()
            };
            newAction._info.AddValue("key", "buttonPress");
            newAction._info.AddValue("buttonPresser", presser.GetInstanceID());
            return newAction;
        }

        public static PlaybackLogEntry CreateButtonUnpress(long timestamp, GameObject subject, GameObject presser)
        {
            PlaybackLogEntry newAction = new PlaybackLogEntry
            {
                timeStamp = timestamp,
                subjectKey = subject.GetInstanceID()
            };
            newAction._info.AddValue("key", "buttonUnpress");
            newAction._info.AddValue("buttonPresser", presser.GetInstanceID());
            return newAction;
        }
        #endregion
        public static PlaybackLogEntry CreateSpawn(long timestamp, GameObject subject, Vector3 position, Quaternion rotation, Vector3 scale)
        {
            int key = subject.GetInstanceID();
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
            //enqueue a function that will perform the serialization of the data at a later time.
            PlaybackLogEntry.spawnQueue.Enqueue(delegate ()
            {
                //Debug.Log(numRunningSerializations);
                PlaybackLogEntry.numRunningSerializations--;
                newAction.SerializeForSpawn(subject, key.ToString());
            });
            if (PlaybackLogEntry.spawner == null)
            {
                PlaybackLogEntry.spawner = PlaybackLogEntry.steadySpawn();
                Dispatcher.queue(PlaybackLogEntry.spawner);
            }
            return newAction;
        }
    }
}