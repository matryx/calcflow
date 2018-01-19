using System.Collections;
using System.Collections.Generic;
using UnityEngine;

#if UNITY_EDITOR
using UnityEditor;
#endif

[ExecuteInEditMode]
public class ClockFixer : MonoBehaviour
{
#if UNITY_EDITOR
    void Awake()
    {

        EditorApplication.update += EditorUpdate;
    }

    static void EditorUpdate()
    {
        if (!EditorApplication.isPlaying || EditorApplication.isPaused)
        {
            if (PlaybackClock.timer.IsRunning)
            {
                print("pausing");
                PlaybackClock.timer.Stop();
            }
        }
        else if (!PlaybackClock.timer.IsRunning)
        {
            print("unpausing");
            PlaybackClock.timer.Start();
        }
    }

#endif
}