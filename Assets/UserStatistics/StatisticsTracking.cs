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

        static string tokenProduction = "5005f6e51ee4fcf77d39f8fc8699e225";
        static string tokenDebug = "a4f806d995f496739c5ad34b8097bf6b";

        public static void Init()
        {
            try
            {
                if (!inited)
                {
                    inited = true;

                    // Log
                    Debug.Log("Starting user statistics");

                    // Generate unique session id
                    session = Guid.NewGuid().ToString();

                    // Create tracking components
                    var trackingObject = new GameObject("Calcflow.UserStatistics.StatisticsTracking");
                    trackingObject.AddComponent<Mixpanel>();
                    trackingObject.AddComponent<ApplicationTracking>();
                    GameObject.DontDestroyOnLoad(trackingObject);

                    // Setup tokens
                    var mixpanel = trackingObject.GetComponent<Mixpanel>();
                    mixpanel.token = tokenProduction;
                    mixpanel.debugToken = tokenDebug;
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
                    machine["Machine"] = Environment.MachineName;
                    machine["Processors"] = Environment.ProcessorCount;
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
            props["Hardware"] = UnityEngine.VR.VRDevice.model;
            foreach (var extra in extras)
            {
                var key = extra.Key;
                var value = extra.Value;
                if (value != null)
                {
                    if (value is double)
                    {
                        props[key] = (double)value;
                    }
                    if (value is long)
                    {
                        props[key] = (long)value;
                    }
                    if (value is int)
                    {
                        props[key] = (int)value;
                    }
                    if (value is short)
                    {
                        props[key] = (short)value;
                    }
                    if (value is float)
                    {
                        props[key] = (float)value;
                    }
                    if (value is byte)
                    {
                        props[key] = (byte)value;
                    }
                    if (value is char)
                    {
                        props[key] = (char)value;
                    }
                    if (value is string)
                    {
                        props[key] = (string)value;
                    }
                    if (value is Quaternion)
                    {
                        var vec = ((Quaternion)value).eulerAngles;
                        props[key + ".x"] = vec.x;
                        props[key + ".y"] = vec.y;
                        props[key + ".z"] = vec.z;
                    }
                    if (value is Vector3)
                    {
                        var vec = (Vector3)value;
                        props[key + ".x"] = vec.x;
                        props[key + ".y"] = vec.y;
                        props[key + ".z"] = vec.z;
                    }
                    if (value is Vector2)
                    {
                        var vec = (Vector2)value;
                        props[key + ".x"] = vec.x;
                        props[key + ".y"] = vec.y;
                    }
                    if (value is Bounds)
                    {
                        var rect = (Bounds)value;
                        props[key + ".center.x"] = rect.center.x;
                        props[key + ".center.y"] = rect.center.y;
                        props[key + ".center.z"] = rect.center.z;
                        props[key + ".size.x"] = rect.size.x;
                        props[key + ".size.y"] = rect.size.y;
                        props[key + ".size.z"] = rect.size.z;
                    }
                    if (value is Rect)
                    {
                        var rect = (Rect)value;
                        props[key + ".center.x"] = rect.center.x;
                        props[key + ".center.y"] = rect.center.y;
                        props[key + ".size.x"] = rect.size.x;
                        props[key + ".size.y"] = rect.size.y;
                    }
                }
            }
            return props;
        }

        delegate void StatsDelegate();
        static void SafeStatsCall(StatsDelegate callback)
        {
            try
            {
                Init();
                if (!tracking)
                {
                    return;
                }
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
