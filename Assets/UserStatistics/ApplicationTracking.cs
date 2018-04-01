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
            StatisticsTracking.StartEvent("Application", "Calcflow", null, false);
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
            StatisticsTracking.StopTracking();
        }

    }

}
