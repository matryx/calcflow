using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LogViewer : MonoBehaviour {

    public List<PlayBackLogAction> log;
    public Recorder rec;
	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
        log = Recorder.recordLog.GetLogCopy();
	}
}
