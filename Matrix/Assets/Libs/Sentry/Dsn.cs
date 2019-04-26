using System.Text.RegularExpressions;
using System;
using UnityEngine;

namespace Unity3DRavenCS
{
    public class DSN
    {
        private bool m_isValid;
        public bool isValid { get { return m_isValid; } }

        private string m_dsnUri;
        private string m_protocol;
        private string m_publicKey;
        private string m_host;
        private int m_projectID;

        public string sentryUri;

        public DSN(string dsnUri)
        {
            m_dsnUri = dsnUri;

            m_isValid = Parse();
        }

        private bool Parse()
        {
            if (string.IsNullOrEmpty(m_dsnUri))
            {
                return false;
            }

            Regex reg = new Regex(@"^(?<protocol>[\w]+)://(?<publicKey>[\w]+)@(?<host>[\w\d.:-_]+)/(?<projectID>[\d]+)[/]?$", RegexOptions.IgnoreCase);
            Match match = reg.Match(m_dsnUri);

            try
            {
                m_protocol = match.Groups["protocol"].Value;
                m_publicKey = match.Groups["publicKey"].Value;
                m_host = match.Groups["host"].Value;
                m_projectID = System.Convert.ToInt32(match.Groups["projectID"].Value);
            }
            catch
            {
                return false;
            }

            this.sentryUri = string.Format("{0}://{1}/api/{2}/store/", m_protocol, m_host, m_projectID);

            return true;
        }

        public string XSentryAuthHeader()
        {
            return string.Format(
                "Sentry sentry_version=7, sentry_client={0}, sentry_timestamp={1}, sentry_key={2}",
                UserAgent(),
                (long)(DateTime.UtcNow - new DateTime(1970, 1, 1)).TotalSeconds,
                m_publicKey
            );
        }

        public string UserAgent()
        {
            return "Unity3DRavenCS/0.0.1";
        }
    }
}
