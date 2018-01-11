using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EnableLogger : MonoBehaviour
{

    void OnDisable()
    {
        Recorder.LogDisable(gameObject);
    }

    void OnEnable()
    {
        Recorder.LogEnable(gameObject);
    }

    void OnDestroy() {
        Recorder.LogDestroy(gameObject);
    }
}
