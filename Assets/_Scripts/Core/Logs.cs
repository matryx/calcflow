using Nanome.Core.Extension;
using System;
using System.Diagnostics;
using System.Reflection;
using System.Text;
using UnityEngine;

namespace Nanome.Core
{
    public enum ELogSeverity
    {
        Debug,
        Warning,
        Exception,
        Error,
        Unknown
    }

    public sealed class LogEntry
    {
        // private setter only for "immutable" class
        // not as strict as readonly but good enough for a DTO 
        public object[] Payload { get; private set; }
        public ELogSeverity Severity { get; private set; }
        public string Channel { get; private set; }
        public bool IsTraced { get; private set; }
        public StackTrace Trace { get; private set; }
        // probably better saving the time/frame stamp since the logging process is threaded
        public DateTime TimeStamp { get; private set; }
        public bool Report { get; private set; }

        private int? _HashCode = null;
        private string _Message = null;
        private string _MessageColor = null;
        private string _MessageLower = null;

        public LogEntry(object[] payload, ELogSeverity severity, string channel, DateTime timeStamp, bool report)
        {
            Payload = payload;
            Severity = severity;
            Channel = channel;
            IsTraced = false;
            TimeStamp = timeStamp;
            Report = report;
        }

        public LogEntry(object[] payload, ELogSeverity severity, string channel, DateTime timeStamp, StackTrace stackTrace, bool report)
        {
            Payload = payload;
            Severity = severity;
            Channel = channel;
            IsTraced = true;
            Trace = stackTrace;
            TimeStamp = timeStamp;
            Report = report;
        }

        public string PrintMessageInLower()
        {
            if (_MessageLower == null)
            {
                _MessageLower = PrintMessagePlain().ToLower();
            }
            return _MessageLower;
        }

        public string PrintMessagePlain()
        {
            if (_Message == null)
            {
                _Message = PrintPayload();
            }
            return _Message;
        }

        public string PrintMessageColor()
        {
            if (_MessageColor == null)
            {
                _MessageColor = PrintPayload(true);
            }
            return _MessageColor;
        }

        public override int GetHashCode()
        {
            if (!_HashCode.HasValue)
            {
                const int multiplier = 17;
                int hash = Severity.GetHashCode();
                hash = hash * multiplier + Channel.GetHashCode();
                hash = hash * multiplier + IsTraced.GetHashCode();
                // go through payload list to get hashcode
                // maybe it's better to get hashcode from the generated message instead
                foreach (object obj in Payload)
                {
                    if (obj != null)
                    {
                        hash = hash * multiplier + obj.GetHashCode();
                    }
                    else
                    {
                        hash *= multiplier;
                    }
                }
                _HashCode = hash;
            }
            return _HashCode.Value;
        }

        public string PrintTimeStamp(bool coloring = false)
        {
            return String.Format("{0:yyyy-MM-dd HH:mm:ss.ffffff (zzz)}", TimeStamp);
        }

        public string PrintSeverity(bool coloring = false)
        {
            string output;
            Color c;
            switch (Severity)
            {
                case ELogSeverity.Debug:
                    output = "[DEBUG]";
                    c = Color.black;
                    break;
                case ELogSeverity.Warning:
                    output = "[WARNING]";
                    c = Color.yellow;
                    break;
                case ELogSeverity.Exception:
                    output = "[EXCEPTION]";
                    c = Color.red;
                    break;
                case ELogSeverity.Error:
                    output = "[ERROR]";
                    c = Color.red;
                    break;
                default:
                    output = "[UNKNOWN]";
                    c = Color.magenta;
                    break;
            }
            if (coloring)
            {
                return ColorString(output, c);
            }
            else
            {
                return output;
            }
        }

        public string PrintChannel(bool coloring = false)
        {
            string output = String.Format("{{{0}}}", Channel);
            if (coloring)
            {
                return RandomColorString(output);
            }
            else
            {
                return output;
            }
        }

        private string PrintPayload(bool coloring = false)
        {
            StringBuilder payloadBuilder = new StringBuilder();
            foreach (object obj in Payload)
            {
                string toString;
                if (obj == null)
                {
                    toString = "null";
                }
                else
                {
                    toString = obj.PrettyPrint();
                }
                if (coloring && !toString.Contains("\n"))
                {
                    payloadBuilder.Append(RandomColorString(toString));
                }
                else
                {
                    payloadBuilder.Append(toString);
                }
                payloadBuilder.Append(" ");
            }
            return payloadBuilder.ToString();
        }

        public string PrintStackTrace(int maxFrames = 0, string leftPadding = "")
        {
            if (!IsTraced)
            {
                return null;
            }
            StackFrame[] frames = Trace.GetFrames();
            int frameCount = frames.Length;
            StringBuilder sb = new StringBuilder();
            if (maxFrames != 0)
            {
                frameCount = Math.Min(frameCount, maxFrames);
            }
            for (int idx = 0; idx < frameCount; ++idx)
            {
                StackFrame frame = frames[idx];
                MethodBase method = frame.GetMethod();
                int iidx = 0;

                // Method info
                sb.Append(leftPadding);
                sb.AppendFormat("{0}:{1}", method.DeclaringType.FullName, method.Name);
                if (method.IsConstructor)
                {
                    sb.Append(".ctor");
                }

                // Generic info
                if (method.IsGenericMethod)
                {
                    Type[] generics = method.GetGenericArguments();
                    sb.Append("<");
                    for (iidx = 0; iidx < generics.Length - 1; iidx++)
                    {
                        sb.Append(generics[iidx].FullName);
                        sb.Append(", ");
                    }
                    sb.Append(generics[iidx].FullName);
                    sb.Append(">");
                }

                // Params info
                sb.Append("(");
                ParameterInfo[] parameters = method.GetParameters();
                if (parameters.Length != 0)
                {
                    for (iidx = 0; iidx < parameters.Length - 1; iidx++)
                    {
                        sb.Append(parameters[iidx].ParameterType.Name);
                        sb.Append(", ");
                    }
                    sb.Append(parameters[iidx].ParameterType.Name);
                }
                sb.Append(")");

                // File info
                sb.AppendFormat(" (at {0}:{1})", frame.GetFileName(), frame.GetFileLineNumber());
                sb.AppendLine();
            }
            return sb.ToString();
        }

        private string ColorString(string src, Color color)
        {
            Color fcolor = Color.black.blend(color, 0.4f);
            if (Nanome.Core.Macros.IsDarkTheme)
            {
                fcolor = Color.white.blend(color, 0.4f);
            }
            return String.Format("<color={0}>{1}</color>", fcolor.toString(), src);
        }

        private string RandomColorString(string src)
        {
            if (src.Length > 50)
            {
                return src;
            }
            float hue = Math.Abs(src.GetHashCode()) % 1000f / 1000f;
            Color color = Color.HSVToRGB(hue, 1f, 1f);
            return ColorString(src, color);
        }

    }

    public class Logs
    {
        public delegate void LogPrintedDelegate(string debugMessage, int severity, string channel);
        public static event LogPrintedDelegate OnLogDebug;

        private static string logFileToday = string.Format("{0:yyyy-MM-dd_HH-mm-ss}", DateTime.Now);

        private static string localLogDirectory()
        {
            return File.Path.inStorage("./History/");
        }

        private static string localLogMessagesPath()
        {
            return File.Path.inStorage("./History/" + logFileToday + "_messages.txt");
        }

        private static string localLogDetailsPath()
        {
            return File.Path.inStorage("./History/" + logFileToday + "_details.txt");
        }

        public static string localHistoryDetailsPath()
        {
            return File.Path.inStorage("./History/" + logFileToday + "_channels.txt");
        }

        private static string directLogMessagesPath()
        {
            return File.Path.inProject(logFileToday + "_messages.txt");
        }

        public static string color(Color32 color)
        {
            string r = Convert.ToString(color.r, 16).ToUpper();
            string g = Convert.ToString(color.g, 16).ToUpper();
            string b = Convert.ToString(color.b, 16).ToUpper();
            string a = Convert.ToString(color.a, 16).ToUpper();
            if (r.Length < 2)
            {
                r = "0" + r;
            }
            if (g.Length < 2)
            {
                g = "0" + g;
            }
            if (b.Length < 2)
            {
                b = "0" + b;
            }
            if (a.Length < 2)
            {
                a = "0" + a;
            }
            string combined = r + g + b + a;
            return "<color=#" + combined + ">#</color>" + combined;
        }

        private static void SaveLog(
            StackTrace stacktrace, object[] payload,
            ELogSeverity severity, string channel,
            DateTime timeStamp,
            bool report = false
        )
        {
            if (disableLogs)
            {
                return;
            }
            LogEntry entry = new LogEntry(payload, severity, channel, timeStamp, stacktrace, report);
            SendLog(entry);
        }

        private static void SaveLog(
            object[] payload,
            ELogSeverity severity, string channel,
            DateTime timeStamp,
            bool report = false
        )
        {
            if (disableLogs)
            {
                return;
            }
            LogEntry entry = new LogEntry(payload, severity, channel, timeStamp, report);
            SendLog(entry);
        }

        private static void SendLog(LogEntry entry)
        {
            if (Macros.Editor)
            {
                Logger.AddLog(
                    String.Format("{0} {1} - {2}", entry.PrintSeverity(true), entry.PrintChannel(true), entry.PrintMessageColor()),
                    (int)entry.Severity,
                    entry.PrintStackTrace(),
                    entry.Channel
                );
            }
            else
            {
                try
                {
                    File.ensureFolderExists(Logs.localLogDirectory());
                    var messagesPath = Logs.localLogMessagesPath();
                    var detailsPath = Logs.localLogDetailsPath();
                    var writeMessage = String.Format(
                        "{0} - {1} {2} {3} {4}",
                        entry.PrintTimeStamp(),
                        entry.PrintSeverity(),
                        entry.PrintChannel(),
                        entry.PrintMessagePlain(),
                        Environment.NewLine
                    );

                    string stackTraceStr = entry.PrintStackTrace(0, "----");

                    if (Daemon.Logging.IsReporting() && entry.Severity >= ELogSeverity.Exception && entry.Report)
                    {
                        Daemon.Logging.Report(writeMessage, entry.Severity, stackTraceStr, entry.Trace);
                    }

                    Nanome.Core.Daemon.Logging.ScheduleForLogging(messagesPath, writeMessage);
                    var writeDetails = string.Format("{0}{1}{2}", writeMessage, stackTraceStr, Environment.NewLine);
                    Nanome.Core.Daemon.Logging.ScheduleForLogging(detailsPath, writeDetails);
                    if (directLogs)
                    {
                        var directPath = Logs.directLogMessagesPath();
                        Nanome.Core.Daemon.Logging.ScheduleForLogging(directPath, writeMessage);
                    }
                }
                catch (Exception e)
                {
                    UnityEngine.Debug.Log("Coult not log to history files:");
                    UnityEngine.Debug.LogException(e);
                }
            }

            if (OnLogDebug != null)
            {
                OnLogDebug.Invoke(entry.PrintMessagePlain(), (int)entry.Severity, entry.Channel);
            }
        }

        static private object[] timedParams(object[] list)
        {
            var nlist = new object[list.Length + 1];
            nlist[0] = string.Format("{0:HH':'mm':'ss'.'fff}", DateTime.Now);
            list.CopyTo(nlist, 1);
            return nlist;
        }

        static private object[] framedParams(object[] list)
        {
            var nlist = new object[list.Length + 1];
            nlist[0] = "[Frame: " + Time.frameCount.ToString() + "]";
            list.CopyTo(nlist, 1);
            return nlist;
        }

        static public void timed(params object[] list)
        {
            StackTrace st = new StackTrace(true);
            DateTime now = DateTime.Now;
            Async.runInMainIfNeeded(delegate ()
            {
                SaveLog(st, timedParams(list), ELogSeverity.Debug, "Default", now);
            });
        }

        static public void timedOnChannel(string channel, params object[] list)
        {
            StackTrace st = new StackTrace(true);
            DateTime now = DateTime.Now;
            Async.runInMainIfNeeded(delegate ()
            {
                SaveLog(st, timedParams(list), ELogSeverity.Debug, channel, now);
            });
        }

        static public void framed(params object[] list)
        {
            StackTrace st = new StackTrace(true);
            DateTime now = DateTime.Now;
            Async.runInMainIfNeeded(delegate ()
            {
                SaveLog(st, framedParams(list), ELogSeverity.Debug, "Default", now);
            });
        }

        static public void framedOnChannel(string channel, params object[] list)
        {
            StackTrace st = new StackTrace(true);
            DateTime now = DateTime.Now;
            Async.runInMainIfNeeded(delegate ()
            {
                SaveLog(st, framedParams(list), ELogSeverity.Debug, channel, now);
            });
        }

        static public void debug(params object[] list)
        {
            StackTrace st = new StackTrace(true);
            DateTime now = DateTime.Now;
            Async.runInMainIfNeeded(delegate ()
            {
                SaveLog(st, list, ELogSeverity.Debug, "Default", now);
            });
        }

        static public void debugOnChannelNoReport(string channel, params object[] list)
        {
            debugOnChannelInternal(channel, false, list);
        }

        static public void debugOnChannel(string channel, params object[] list)
        {
            debugOnChannelInternal(channel, true, list);
        }

        static private void debugOnChannelInternal(string channel, bool report, params object[] list)
        {
            StackTrace st = new StackTrace(true);
            DateTime now = DateTime.Now;
            Async.runInMainIfNeeded(delegate ()
            {
                SaveLog(st, list, ELogSeverity.Debug, channel, now, report);
            });
        }

        static public void warning(params object[] list)
        {
            StackTrace st = new StackTrace(true);
            DateTime now = DateTime.Now;
            Async.runInMainIfNeeded(delegate ()
            {
                SaveLog(st, list, ELogSeverity.Warning, "Default", now);
            });
        }

        static public void warningOnChannelNoReport(string channel, params object[] list)
        {
            warningOnChannelInternal(channel, false, list);
        }

        static public void warningOnChannel(string channel, params object[] list)
        {
            warningOnChannelInternal(channel, true, list);
        }

        static private void warningOnChannelInternal(string channel, bool report, params object[] list)
        {
            StackTrace st = new StackTrace(true);
            DateTime now = DateTime.Now;
            Async.runInMainIfNeeded(delegate ()
            {
                SaveLog(st, list, ELogSeverity.Warning, channel, now, report);
            });
        }

        static public void exception(params object[] list)
        {
            StackTrace st = new StackTrace(true);
            DateTime now = DateTime.Now;
            Async.runInMainIfNeeded(delegate ()
            {
                SaveLog(st, list, ELogSeverity.Exception, "Default", now);
            });
        }

        static public void exceptionOnChannelNoReport(string channel, params object[] list)
        {
            exceptionOnChannelInternal(channel, false, list);
        }

        static public void exceptionOnChannel(string channel, params object[] list)
        {
            exceptionOnChannelInternal(channel, true, list);
        }

        static private void exceptionOnChannelInternal(string channel, bool report, params object[] list)
        {
            StackTrace st = new StackTrace(true);
            DateTime now = DateTime.Now;
            Async.runInMainIfNeeded(delegate ()
            {
                SaveLog(st, list, ELogSeverity.Exception, channel, now, report);
            });
        }

        static public void error(params object[] list)
        {
            StackTrace st = new StackTrace(true);
            DateTime now = DateTime.Now;
            Async.runInMainIfNeeded(delegate ()
            {
                SaveLog(st, list, ELogSeverity.Error, "Default", now);
            });
        }

        static public void errorOnChannelNoReport(string channel, params object[] list)
        {
            errorOnChannelInternal(channel, false, list);
        }

        static public void errorOnChannel(string channel, params object[] list)
        {
            errorOnChannelInternal(channel, true, list);
        }

        static private void errorOnChannelInternal(string channel, bool report, params object[] list)
        {
            StackTrace st = new StackTrace(true);
            DateTime now = DateTime.Now;
            Async.runInMainIfNeeded(delegate ()
            {
                SaveLog(st, list, ELogSeverity.Error, channel, now, report);
            });
        }

        static public void stacktrace()
        {
            try
            {
                throw new Exception("StackTrace");
            }
            catch (Exception e)
            {
                Logs.exception(e);
            }
        }

        static readonly bool disableLogs = Nanome.Core.Config.getBool("runtime-no-logs", "false");
        static readonly bool directLogs = Nanome.Core.Config.getBool("runtime-direct-logs", "false");

    }

}
