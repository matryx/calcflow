using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LogViewer : MonoBehaviour
{
    public int numCurrentSpawns;

    public int replayLogCount;
    public int recordLogCount;

    public bool displayLog;

    public PlaybackLogEntry[] actionLog;


    // Use this for initialization
    void Start()
    {

    }

    // Update is called once per frame
    void Update()
    {
        numCurrentSpawns = Recorder.SpawnQueueSize();
        if (Replayer.log != null)
        {
            replayLogCount = Replayer.log.Count;
        }
        if (Recorder.debugRecordLog != null)
        {
            recordLogCount = Recorder.debugRecordLog.log.Count;
        }

        if (displayLog)
        {
            displayLog = false;
            actionLog = Recorder.debugRecordLog.log.ToArray();
        }

    }
}
