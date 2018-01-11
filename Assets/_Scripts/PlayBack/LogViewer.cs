using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LogViewer : MonoBehaviour {
    public int numCurrentSpawns;
    public List<PlaybackLogAction2> log;
    public Recorder rec;
	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
        numCurrentSpawns = PlayBackLogAction.numRunningSerializations;
        //if (numCurrentSpawns > 0) print(numCurrentSpawns);
        log = Recorder.recordLog.GetLogCopy();
    }
}
