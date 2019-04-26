using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.Analytics;

public class TimeSpentOnScene : MonoBehaviour {

    float startTime = 0f;
    float endTime = 0f;
    float totalTime = 0f;

    void Awake()
    {
        startTime = Time.time;
    }

    void CalculateTimeSpent()
    {
        endTime = Time.time;
        totalTime = endTime - startTime;
        Analytics.CustomEvent("Time on " + SceneManager.GetActiveScene().name, new Dictionary<string, object>
        {
            {"start time", startTime},
            {"end time", endTime},
            {"total time", totalTime}
        });
    }
    void OnDestroy()
    {
        CalculateTimeSpent();
    }

    void OnApplicationQuit() {
        CalculateTimeSpent();
        Analytics.CustomEvent("Total Time Application Run", new Dictionary<string, object> {
            { "time", endTime }
        });
    }
}
