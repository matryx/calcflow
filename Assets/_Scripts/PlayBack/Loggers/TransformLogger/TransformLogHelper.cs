using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TransformLogHelper : MonoBehaviour
{
    public TransformLogHelper _instance;

    public delegate void TransformLog(long frameTime);
    private static event TransformLog LogTransforms;
    private static event TransformLog LateLogTransforms;

    void Awake()
    {
        _instance = this;
    }
    long frameTime = 0;
    // void Update()
    // {
    //     frameTime = PlaybackClock.GetTime();
    // }
    void LateUpdate()
    {
        frameTime = PlaybackClock.GetTime();
        foreach (ReenactableLoggerTransform logger in ReenactableLoggerTransform.AllTransformLoggers)
        {
            if (logger.transform.parent == null)
            {
                logger.RecursiveRecordPosition(frameTime);
            }
        }
    }


    public static int GetDepth(Transform target)
    {
        Transform cTransform = target;
        int depth = 0;
        while (cTransform.parent != null)
        {
            depth++;
            cTransform = cTransform.parent;
        }
        return depth;
    }
}
