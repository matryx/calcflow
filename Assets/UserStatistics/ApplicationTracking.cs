using UnityEngine;
using UnityEngine.SceneManagement;

using System;
using System.Collections;
using System.Collections.Generic;

namespace Calcflow.UserStatistics
{

    public class ApplicationTracking : MonoBehaviour
    {

        void Awake()
        {
            Debug.LogWarning("OnAwake");
            StatisticsTracking.StartEvent("Application", "Calcflow");
            SceneManager.activeSceneChanged += SceneChanged;
        }

        void SceneChanged(Scene previousScene, Scene nextScene)
        {
            StatisticsTracking.EndAllStartedEvents();
            if (previousScene.IsValid())
            {
                StatisticsTracking.EndEvent("Scene", previousScene.name);
            }
            StatisticsTracking.StartEvent("Scene", nextScene.name);
        }

        void OnApplicationQuit()
        {
            StatisticsTracking.EndEvent("Application", "Calcflow");
            StatisticsTracking.Flush();
        }

    }

}
