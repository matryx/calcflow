using UnityEngine;
using UnityEngine.SceneManagement;

using System;
using System.Collections;
using System.Collections.Generic;
using System.Net.NetworkInformation;
using System.Linq;

using mixpanel;

namespace Calcflow.UserStatistics
{

    public class StatisticsTracking
    {

        static Dictionary<string, double> startTimes = new Dictionary<string, double>();

        static bool inited = false;
        static bool tracking = false;

        static string session = "--";

        static string mac = "??";
        static string host = "??";
        static string user = "??-??";

        public static void Init()
        {
            try
            {
                if (!inited)
                {
                    inited = true;

                    // Generate unique session id
                    session = Guid.NewGuid().ToString();

                    // Create tracking components
                    var trackingObject = new GameObject("Calcflow.UserStatistics.StatisticsTracking");
                    trackingObject.AddComponent<Mixpanel>();
                    trackingObject.AddComponent<ApplicationTracking>();
                    GameObject.DontDestroyOnLoad(trackingObject);

                    // Setup tokens
                    var mixpanel = trackingObject.GetComponent<Mixpanel>();
                    mixpanel.token = "5005f6e51ee4fcf77d39f8fc8699e225";
                    mixpanel.debugToken = "a4f806d995f496739c5ad34b8097bf6b";
                    mixpanel.trackInEditor = true;

                    // Mac addresses
                    var macAddresses =
                    (
                        from nic in NetworkInterface.GetAllNetworkInterfaces()
                        where nic.OperationalStatus == OperationalStatus.Up
                        select nic.GetPhysicalAddress().ToString()
                    );

                    // Session infos
                    mac = macAddresses.FirstOrDefault();
                    host = System.Net.Dns.GetHostName();
                    user = host + ":" + mac;

                    // Ready to track
                    tracking = true;

                    // Machine details
                    var machine = new Value();
                    machine["Mac"] = mac;
                    machine["Host"] = host;
                    machine["Unique"] = user;
                    // User infos
                    StatsIdentify(user, host, machine);
                }
            }
            catch (Exception e)
            {
                Debug.LogError("Could not initialize user tracking: " + e);
            }
        }

        public static void InstantEvent(string eventType, string eventName, Dictionary<string, object> extras = null)
        {
            SafeStatsCall(delegate ()
            {
                // Log
                Debug.Log("EventTracking Instant: " + eventType + " -> " + eventName);
                // Build props
                var props = EventProperties(eventName, "Unique", extras);
                // Send stats
                StatsTrack(eventType, props);
            });
        }

        public static void StartEvent(string eventType, string eventName, Dictionary<string, object> extras = null)
        {
            SafeStatsCall(delegate ()
            {
                // Log
                Debug.Log("EventTracking Start: " + eventType + " -> " + eventName);
                // Build props
                var props = EventProperties(eventName, "Start", extras);
                // Event key
                var key = eventType + ":" + eventName;
                // Save start time
                lock (startTimes)
                {
                    startTimes[key] = Now();
                }
                // Send stats
                StatsTrack(eventType + " Start", props);
            });
        }

        public static void EndEvent(string eventType, string eventName, Dictionary<string, object> extras = null)
        {
            SafeStatsCall(delegate ()
            {
                // Log
                Debug.Log("EventTracking End: " + eventType + " -> " + eventName);
                // Build props
                var props = EventProperties(eventName, "End", extras);
                // Event key
                var key = eventType + ":" + eventName;
                // Get duration
                double startTime;
                lock (startTimes)
                {
                    if (startTimes.TryGetValue(key, out startTime))
                    {
                        props["Duration"] = startTime;
                        startTimes.Remove(key);
                    }
                }
                // Send stats
                StatsTrack(eventType + " End", props);
            });
        }

        public static void Flush()
        {
            SafeStatsCall(delegate()
            {
                // Log
                Debug.Log("EventTracking, Flushing");
                // Flush
                StatsFlush();
            });
        }

        public static void EndAllStartedEvents()
        {
            SafeStatsCall(delegate ()
            {
                // Log
                Debug.Log("EventTracking, Clearing pending started events");
                // Fetch non finished events
                var keys = new List<string>();
                lock (startTimes)
                {
                    foreach (var key in startTimes.Keys)
                    {
                        keys.Add(key);
                    }
                    startTimes.Clear();
                }
                // Loop over non-finished events
                foreach (var key in keys)
                {
                    var parts = key.Split(new char[] { ':' });
                    if (parts.Length >= 2)
                    {
                        EndEvent(parts[0], parts[1]);
                    }
                }
            });
        }

        static Value EventProperties(string eventName, string eventStage, Dictionary<string, object> extras = null)
        {
            var props = new Value();
            props["Name"] = eventName;
            props["Stage"] = eventStage;
            props["Scene"] = SceneManager.GetActiveScene().name;
            props["Mac"] = mac;
            props["User"] = user;
            props["Host"] = host;
            props["Session"] = session;
            return props;
        }

        delegate void StatsDelegate();
        static void SafeStatsCall(StatsDelegate callback)
        {
            try
            {
                if (!tracking)
                {
                    return;
                }
                Init();
                callback();
            }
            catch (Exception e)
            {
                Debug.LogError("Could not track event: " + e);
            }
        }

        static void StatsTrack(string name, Value props)
        {
            if (tracking)
            {
                Mixpanel.Track(name, props);
            }
        }

        static void StatsFlush()
        {
            if (tracking)
            {
                Mixpanel.FlushQueue();
            }
        }

        static void StatsIdentify(string id, string name, Value props)
        {
            if (tracking)
            {
                Mixpanel.Identify(id.ToString());
                Mixpanel.people.Name = name.ToString();
                Mixpanel.people.Set(props);
            }
        }

        static double Now()
        {
            var utcTime = DateTime.Now.ToUniversalTime();
            var epochTime = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
            var relativeTime = utcTime.Subtract(epochTime);
            return relativeTime.TotalSeconds;
        }

    }

}
