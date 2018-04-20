using UnityEngine;

using System.Collections;
using System.Collections.Generic;

namespace Calcflow.UserStatistics
{

    public class ObjectTracking : MonoBehaviour
    {

        public string objectName;

        string GetObjectName()
        {
            var finalName = objectName;
            if (finalName == null || finalName == "")
            {
                finalName = gameObject.name;
            }
            return finalName;
        }

        void Awake()
        {
            StatisticsTracking.StartEvent("Object Life", GetObjectName());
        }

        void OnDestroy()
        {
            StatisticsTracking.EndEvent("Object Life", GetObjectName());
        }

        void OnEnable()
        {
            StatisticsTracking.StartEvent("Object Active", GetObjectName());
        }

        void OnDisable()
        {
            StatisticsTracking.EndEvent("Object Active", GetObjectName());
        }

    }

}