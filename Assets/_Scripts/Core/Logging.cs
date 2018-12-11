using UnityEngine;

using System;
using System.IO;
using System.Collections.Generic;

using Nanome.Core;
using Nanome.Core.Importer;
using Nanome.Core.Extension;

namespace Nanome.Core.Daemon
{

    public class Logging : Nanome.Core.Behaviour
    {

        // TODO: Ideally that url should be returned by API, so we can change it whenever we want
        private const string SentryReportUrl = "https://449658ec1d514b46b09ddd924a09de9f@sentry.io/1334722";

        private static Dictionary<string, List<string>> s_PendingLogs = new Dictionary<string, List<string>>();

        private static Unity3DRavenCS.Unity3DRavenCS s_Client;
        private static Dictionary<string, string> s_Tags;
        private static Dictionary<ELogSeverity, HashSet<string>> s_SentCallstacks;

        private static ThrottleTimer s_HistoryTimer = new ThrottleTimer(5.0f);
        private static ThrottleTimer s_PendingTimer = new ThrottleTimer(0.5f);

        void Awake()
        {
            Application.logMessageReceived += HandleUnhandledMessages;
            if (Macros.Logging)
            {
                Unity3DRavenCS.Unity3DRavenCS.NewInstance(SentryReportUrl);
                s_Client = Unity3DRavenCS.Unity3DRavenCS.instance;
                s_Tags = new Dictionary<string, string>();
                s_Tags.Add("Device-Model", SystemInfo.deviceModel);
                s_Tags.Add("Device-Name", SystemInfo.deviceName);
                s_Tags.Add("OS", SystemInfo.operatingSystem);
                s_Tags.Add("MemorySize", SystemInfo.systemMemorySize.ToString());
                s_Tags.Add("Version", Nanome.Core.Version.Current);
                s_SentCallstacks = new Dictionary<ELogSeverity, HashSet<string>>();
                ELogSeverity[] values = (ELogSeverity[])Enum.GetValues(typeof(ELogSeverity));
                foreach (ELogSeverity value in values)
                {
                    s_SentCallstacks.Add(value, new HashSet<string>());
                }
            }
        }

        void HandleUnhandledMessages(string condition, string stackTrace, LogType type)
        {
            if (type == LogType.Log)
            {
                Logs.debugOnChannel("Unity", condition, stackTrace);
            }
            if (type == LogType.Warning)
            {
                Logs.warningOnChannel("Unity", condition, stackTrace);
            }
            if (type == LogType.Exception)
            {
                if (s_Client != null && s_SentCallstacks[ELogSeverity.Exception].Add(stackTrace))
                {
                    s_Client.CaptureException(condition, stackTrace, s_Tags);
                }
                Logs.exceptionOnChannelNoReport("Unity", condition, stackTrace);
            }
            if (type == LogType.Error || type == LogType.Assert)
            {
                if (s_Client != null && s_SentCallstacks[ELogSeverity.Error].Add(stackTrace))
                {
                    s_Client.CaptureException(condition, stackTrace, s_Tags);
                }
                Logs.errorOnChannelNoReport("Unity", condition, stackTrace);
            }
        }

        void Update()
        {
            if (Macros.Editor)
            {
                return;
            }
            FlushLogsIfNeeded();
            FlushHistoryIfNeeded();
        }

        public static void ScheduleForLogging(string path, string str)
        {
            lock (s_PendingLogs)
            {
                if (!s_PendingLogs.ContainsKey(path))
                {
                    s_PendingLogs[path] = new List<string>();
                }
                s_PendingLogs[path].Add(str);
            }
        }

        public static void FlushLogsIfNeeded()
        {
            // Only trigger flush on a time basis
            s_PendingTimer.trigger(delegate ()
            {
                // Log in a thread to not clog the main thread with file writing
                Async.runInThread(delegate (Async t)
                {
                    FlushLogsNow();
                });
            });
        }

        public static void FlushLogsNow()
        {
            lock (s_PendingLogs)
            {
                foreach (var path in new List<string>(s_PendingLogs.Keys))
                {
                    // Concat all lines to be logged
                    try
                    {
                        var str = s_PendingLogs[path].Join<string>("");
                        Nanome.Core.File.writeFileAppend(path, str);
                        s_PendingLogs.Remove(path);
                    }
                    catch (Exception e)
                    {
                        Debug.Log("Error while writing to log files:" + e);
                    }
                }
            }
        }

        public static void FlushHistoryIfNeeded()
        {
            s_HistoryTimer.trigger(delegate ()
            {
                Async.runInThread(delegate (Async t)
                {
                    var lines = Nanome.Core.Communication.history();
                    var path = Nanome.Core.Logs.localHistoryDetailsPath();
                    Nanome.Core.File.writeFileLines(path, lines);
                });
            });
        }

        public static bool IsReporting()
        {
            return s_Client != null;
        }

        public static void Report(string message, ELogSeverity severity, string stackTraceStr, System.Diagnostics.StackTrace stackTrace)
        {
            if (s_SentCallstacks[severity].Add(stackTraceStr))
            {
                s_Client.CaptureMessageWithSysStack(message, NanomeLogTypeToUnityLogType(severity), s_Tags, stackTrace);
            }
        }

        private static LogType NanomeLogTypeToUnityLogType(ELogSeverity severity)
        {
            switch (severity)
            {
                case ELogSeverity.Debug:
                    return LogType.Log;
                case ELogSeverity.Warning:
                    return LogType.Warning;
                case ELogSeverity.Exception:
                    return LogType.Exception;
                case ELogSeverity.Error:
                    return LogType.Error;
            }
            return LogType.Log;
        }

    }

}
