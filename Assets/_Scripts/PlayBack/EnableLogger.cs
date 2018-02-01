using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EnableLogger : MonoBehaviour
{

    void OnDisable()
    {
        if (Replayer.Replaying)
            Recorder.LogDisable(gameObject);
    }

    void OnEnable()
    {
        if (Replayer.Replaying)
            Recorder.LogEnable(gameObject);
    }

    void OnDestroy()
    {
        if (Replayer.Replaying)
            Recorder.LogDestroy(gameObject);
    }
}
