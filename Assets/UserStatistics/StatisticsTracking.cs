using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;

namespace Calcflow.UserStatistics
{

    public class StatisticsTracking
    {

        public static void StartEvent(string eventType, string eventName, Dictionary<string, object> extras = null)
        {
            Debug.Log("EventTracking Start: " + eventType + " -> " + eventName);
            // TODO
        }

        public static void EndEvent(string eventType, string eventName, Dictionary<string, object> extras = null)
        {
            Debug.Log("EventTracking End: " + eventType + " -> " + eventName);
            // TODO
        }

    }

}
