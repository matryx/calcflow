using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RecordLogViewer : MonoBehaviour {

    PlaybackLog log;
    [SerializeField]
    List<PlayBackLogAction> list;

    private void Start()
    {
        log = Recorder.recordLog;
    }

    // Update is called once per frame
    void Update () {
        list = log.log;
	}
}
