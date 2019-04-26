using System;
using Newtonsoft.Json;

namespace Unity3DRavenCS
{
    public class RavenException
    {
#pragma warning disable 0414
        [JsonProperty(PropertyName = "stacktrace")]
        private RavenStackTrace m_stacktrace;
        [JsonProperty(PropertyName = "value")]
        private string m_value;
        [JsonProperty(PropertyName = "type")]
        private string m_type;
#pragma warning restore 0414

        public RavenException(Exception exception)
        {
            m_stacktrace = new RavenStackTrace(exception);
            m_value = exception.Message;
            m_type = exception.GetType().ToString();
        }

        public RavenException(string message, string stackTrace)
        {
            m_stacktrace = new RavenStackTrace(stackTrace);
            m_value = message;
            m_type = message;
        }

        public RavenException(string message, System.Diagnostics.StackTrace stackTrace)
        {
            m_stacktrace = new RavenStackTrace(stackTrace);
            m_value = message;
            m_type = message;
        }
    }
}