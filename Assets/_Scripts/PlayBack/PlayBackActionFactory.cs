using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Nanome.Core;
using Nanome.Core.Daemon;
using Extensions;

public partial class PlaybackLogAction2
{
    public class PlayBackActionFactory
    {

        #region other constructors
        public static PlaybackLogAction2 CreateMovement(long timestamp, long duration, GameObject subject, Vector3 destination, Quaternion rotation, Vector3 scale, GameObject parent)
        {
            int parentKey = (parent == null) ? 0 : parent.GetInstanceID();
            int key = subject.GetInstanceID();
            PlaybackLogAction2 newAction = new PlaybackLogAction2
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

        public static PlaybackLogAction2 CreateEnable(long timestamp, GameObject subject)
        {
            PlaybackLogAction2 newAction = new PlaybackLogAction2
            {
                timeStamp = timestamp,
                subjectKey = subject.GetInstanceID(),
            };
            newAction._info.AddValue("key", "enable");

            return newAction;
        }

        public static PlaybackLogAction2 CreateDisable(long timestamp, GameObject subject)
        {
            PlaybackLogAction2 newAction = new PlaybackLogAction2
            {
                timeStamp = timestamp,
                subjectKey = subject.GetInstanceID(),
            };
            newAction._info.AddValue("key", "disable");

            return newAction;
        }

        public static PlaybackLogAction2 CreateDestroy(long timestamp, GameObject subject)
        {
            PlaybackLogAction2 newAction = new PlaybackLogAction2
            {
                timeStamp = timestamp,
                subjectKey = subject.GetInstanceID(),
            };
            newAction._info.AddValue("key", "destroy");

            return newAction;
        }

        public static PlaybackLogAction2 CreateButtonPress(long timestamp, GameObject subject, GameObject presser)
        {
            PlaybackLogAction2 newAction = new PlaybackLogAction2
            {
                timeStamp = timestamp,
                subjectKey = subject.GetInstanceID()
            };
            newAction._info.AddValue("key", "buttonPress");
            newAction._info.AddValue("buttonPresser", presser.GetInstanceID());
            return newAction;
        }

        public static PlaybackLogAction2 CreateButtonUnpress(long timestamp, GameObject subject, GameObject presser)
        {
            PlaybackLogAction2 newAction = new PlaybackLogAction2
            {
                timeStamp = timestamp,
                subjectKey = subject.GetInstanceID()
            };
            newAction._info.AddValue("key", "buttonUnpress");
            newAction._info.AddValue("buttonPresser", presser.GetInstanceID());
            return newAction;
        }
        #endregion
        public static PlaybackLogAction2 CreateSpawn(long timestamp, GameObject subject, Vector3 position, Quaternion rotation, Vector3 scale)
        {
            int key = subject.GetInstanceID();
            PlaybackLogAction2 newAction = new PlaybackLogAction2
            {
                timeStamp = timestamp,
                subjectKey = key
            };
            newAction._info.AddValue("key", "spawn");
            newAction._info.AddValue("name", subject.name);
            newAction._info.AddValue("position", position);
            newAction._info.AddValue("rotation", rotation);
            newAction._info.AddValue("scale", scale);

            PlaybackLogAction2.numRunningSerializations++;
            //enqueue a function that will perform the serialization of the data at a later time.
            PlaybackLogAction2.spawnQueue.Enqueue(delegate ()
            {
                //Debug.Log(numRunningSerializations);
                PlaybackLogAction2.numRunningSerializations--;
                newAction.SerializeForSpawn(subject, key.ToString());
            });
            if (PlaybackLogAction2.spawner == null)
            {
                PlaybackLogAction2.spawner = PlaybackLogAction2.steadySpawn();
                Dispatcher.queue(PlaybackLogAction2.spawner);
            }
            return newAction;
        }
    }
}