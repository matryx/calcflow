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
public class PlaybackLog
{
    public const long Period = 30;

    [SerializeField]
    public long period = 30;

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

        //System.IO.File.WriteAllText(Path.Combine(savePath, fileName) + jsonExtension, JsonUtility.ToJson(recordingInstance.log));

    }

    [SerializeField]
    public List<PlaybackLogEntry> log = new List<PlaybackLogEntry>();
    public List<PlaybackLogEntry> GetLogCopy()
    {
        //This sort removes a bug caused by the way movement is logged. 
        //Movement's time is logged as 1 timer period earlier than it actually happened to improve the accuracy of the lerp.
        //This doesn't work if it gets stuck behind something that isn't logged early since the recorder goes in list order.
        log.Sort(new myReverserClass());
        return log.ToList();
    }

    public class myReverserClass : IComparer<PlaybackLogEntry>
    {
        public int Compare(PlaybackLogEntry x, PlaybackLogEntry y)
        {
            if (x.timeStamp > y.timeStamp) return 1;
            if (x.timeStamp < y.timeStamp) return -1;
            return 0;
        }
    }

    public void Add(PlaybackLogEntry action)
    {
        log.Add(action);
    }

}
