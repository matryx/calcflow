using UnityEngine;
using UnityEngine.SceneManagement;

using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Net.NetworkInformation;
using System.Linq;

namespace Calcflow.UserStatistics
{

    public class StatisticsTracking
    {

        static Dictionary<string, double> startTimesSaved = new Dictionary<string, double>();
        static Dictionary<string, double> startTimes = new Dictionary<string, double>();

        static bool inited = false;
        static bool tracking = false;

        static string session = "--";

        static string mac = "??";
        static string host = "??";
        static string user = "??-??";

        static string tokenProduction = "5005f6e51ee4fcf77d39f8fc8699e225";
        static string tokenDebug = "c6bd5ad95175ef01b440a5baaf8b49d2";
        static string token = "";

        static ApplicationTracking tracker = null;

        public static void Init()
        {
            try
            {
                if (!inited)
                {
                    inited = true;

                    // Log
                    ColorPrint("Starting user statistics");

                    // Generate unique session id
                    session = Guid.NewGuid().ToString();

                    // Choose a token
#if UNITY_EDITOR
                    token = tokenDebug;
#else
                    token = tokenProduction;
#endif

                    // Create tracking components
                    var trackingObject = new GameObject("Calcflow.UserStatistics.StatisticsTracking");
                    trackingObject.AddComponent<ApplicationTracking>();
                    GameObject.DontDestroyOnLoad(trackingObject);

                    tracker = trackingObject.GetComponent<ApplicationTracking>();

                    // Ready to track
                    tracking = true;

                    // Log
                    ColorPrint("Initialized tracking objects");

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

                    // Log
                    ColorPrint("Read user infos");

                    // Machine details
                    var machine = new Dictionary<string, object>();
                    machine["Mac"] = mac;
                    machine["Host"] = host;
                    machine["Unique"] = user;
                    machine["Machine"] = Environment.MachineName;
                    machine["Processors"] = Environment.ProcessorCount;
                    // User infos
                    StatsIdentify(user, host, machine);

                    // Log
                    ColorPrint("Start the tracking");
                }
            }
            catch (Exception e)
            {
                Debug.LogError("Could not initialize user tracking: " + e);
            }
        }

        public static void StopTracking()
        {
            tracking = false;
        }

        public static void InstantEvent(string eventType, string eventName, Dictionary<string, object> extras = null)
        {
            SafeStatsCall(delegate ()
            {
                // Log
                ColorPrint("EventTracking Instant: " + eventType + " -> " + eventName);
                // Build props
                var props = EventProperties(eventName, "Unique", extras);
                // Send stats
                StatsTrack(eventType, props);
            });
        }

        public static void StartEvent(string eventType, string eventName, Dictionary<string, object> extras = null, bool clearable = true)
        {
            SafeStatsCall(delegate ()
            {
                // Log
                ColorPrint("EventTracking Start: " + eventType + " -> " + eventName);
                // Build props
                var props = EventProperties(eventName, "Start", extras);
                // Event key
                var key = eventType + ":" + eventName;
                // Save start time
                if (clearable)
                {
                    lock (startTimes)
                    {
                        startTimes[key] = Now();
                    }
                }
                else
                {
                    lock (startTimesSaved)
                    {
                        startTimesSaved[key] = Now();
                    }
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
                ColorPrint("EventTracking End: " + eventType + " -> " + eventName);
                // Build props
                var props = EventProperties(eventName, "End", extras);
                // Event key
                var key = eventType + ":" + eventName;
                // Get duration
                double duration = 0;
                lock (startTimes)
                {
                    double startTime;
                    if (startTimes.TryGetValue(key, out startTime))
                    {
                        duration = (Now() - startTime);
                        startTimes.Remove(key);
                    }
                }
                lock (startTimesSaved)
                {
                    double startTime;
                    if (startTimesSaved.TryGetValue(key, out startTime))
                    {
                        duration = (Now() - startTime);
                        startTimesSaved.Remove(key);
                    }
                }
                // Save duration
                props["Duration"] = duration;
                // Send stats
                StatsTrack(eventType + " End", props);
            });
        }

        public static void EndAllStartedEvents()
        {
            SafeStatsCall(delegate ()
            {
                // Log
                ColorPrint("EventTracking, Clearing pending started events");
                // Fetch non finished events
                var keys = new List<string>();
                lock (startTimes)
                {
                    foreach (var key in startTimes.Keys)
                    {
                        keys.Add(key);
                    }
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

        static Dictionary<string, object> EventProperties(string eventName, string eventStage, Dictionary<string, object> extras = null)
        {
            var props = new Dictionary<string, object>();
            props["Name"] = eventName;
            props["Stage"] = eventStage;
            props["Scene"] = SceneManager.GetActiveScene().name;
            props["User"] = user;
            props["Session"] = session;
            props["Hardware"] = UnityEngine.XR.XRDevice.model;
            if (extras != null)
            {
                foreach (var extra in extras)
                {
                    var key = extra.Key;
                    var value = extra.Value;
                    if (value != null)
                    {
                        if (value is bool)
                        {
                            props[key] = (bool)value;
                        }
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

        static void StatsTrack(string name, Dictionary<string, object> props)
        {
            if (tracking)
            {
                var jsonObject = new Dictionary<string, object>();
                jsonObject["event"] = name;
                var jsonProperties = props;
                jsonProperties["token"] = token;
                jsonProperties["distinct_id"] = user;
                jsonObject["properties"] = jsonProperties;
                var dataString = MakeDataString(jsonObject);
                SendDataString("http://api.mixpanel.com/track/?data=", dataString);
            }
        }

        static void StatsIdentify(string id, string name, Dictionary<string, object> props)
        {
            if (tracking)
            {
                var jsonObject = new Dictionary<string, object>();
                jsonObject["$token"] = token;
                jsonObject["$distinct_id"] = id;
                jsonObject["$set"] = props;
                props["$name"] = name;
                var dataString = MakeDataString(jsonObject);
                SendDataString("http://api.mixpanel.com/engage/?data=", dataString);
            }
        }

        static double Now()
        {
            var utcTime = DateTime.Now.ToUniversalTime();
            var epochTime = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
            var relativeTime = utcTime.Subtract(epochTime);
            return relativeTime.TotalSeconds;
        }

        static string MakeDataString(Dictionary<string, object> jsonObject)
        {
            var jsonString = jsonWriter.SerializeString(jsonObject);
            var bytesToEncode = Encoding.UTF8.GetBytes(jsonString);
            var encodedText = Convert.ToBase64String(bytesToEncode);
            return encodedText;
        }

        static void SendDataString(string url, string data)
        {
            tracker.DoRequest(url, data);
        }

        static Nanome.Maths.Serializers.JsonSerializer.Serializer jsonWriter = new Nanome.Maths.Serializers.JsonSerializer.Serializer();

        private static void ColorPrint(string toPrint)
        {
            string prepend = "<color=#707000>";
            string append = "</color>";

            Debug.Log(prepend + toPrint + append);
        }

    }

}
