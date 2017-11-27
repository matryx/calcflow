using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LogViewer : MonoBehaviour {
    public int numCurrentSpawns;
    public bool spawning;
    public List<PlayBackLogAction> log;
    public Recorder rec;
	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
        spawning = PlayBackLogAction.recordingSpawns;
        if (spawning == true) Debug.Log(spawning);
        numCurrentSpawns = PlayBackLogAction.numRunningSerializations;
        if (numCurrentSpawns > 0) print(numCurrentSpawns);
        log = Recorder.recordLog.GetLogCopy();
    }
}
