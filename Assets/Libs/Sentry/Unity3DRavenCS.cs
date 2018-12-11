using UnityEngine;
using System.IO;
using System;
using System.Collections;
using System.Collections.Generic;
using Newtonsoft.Json;
using ICSharpCode.SharpZipLib.GZip;

namespace Unity3DRavenCS
{

    public class RavenOptionType
    {
        public int timeout = 5000;
        public bool compression = true;
        public int maxConnections = 10;
    }


    public class Unity3DRavenCS : MonoBehaviour
    {
        public static Unity3DRavenCS instance
        {
            get
            {
                if (m_instance == null)
                {
                    Debug.LogError("Unity3DRavenCS is not initialized. Please call Unity3DRavenCS.NewInstance() before using it.");
                }

                return m_instance;
            }
        }
        private static Unity3DRavenCS m_instance;

        public static void NewInstance(string dsn, RavenOptionType option = null)
        {
            if (m_instance == null)
            {
                GameObject obj = new GameObject("RavenObj");
                obj.AddComponent<Unity3DRavenCS>();
                m_instance = obj.GetComponent<Unity3DRavenCS>();

                m_instance.m_dsn = new DSN(dsn);
                if (!m_instance.m_dsn.isValid)
                {
                    m_instance.m_valid = false;
                    Debug.Log("Unity3DRavenCS is disabled because the DSN is invalid.");
                }
                else
                {
                    m_instance.m_valid = true;
                }

                m_instance.m_option = option == null ? new RavenOptionType() : option;

                m_instance.Init();

                DontDestroyOnLoad(obj);
            }
        }

        private DSN m_dsn;
        private bool m_valid;
        private RavenOptionType m_option;

        private List<HttpClient> m_requestList;
        private int m_lastIdx;

        private void Init()
        {
            m_requestList = new List<HttpClient>();
            for (int i = 0; i < m_option.maxConnections; ++i)
            {
                m_requestList.Add(null);
            }
            m_lastIdx = 0;
        }

        void Start()
        {
        }

        void Update()
        {
            for (int idx = 0; idx < m_requestList.Count; idx++)
            {
                HttpClient httpClient = m_requestList[idx];
                if (httpClient != null)
                {
                    if (httpClient.www.isDone)
                    {
                        // ResponsePacket responsePacket = JsonConvert.DeserializeObject<ResponsePacket>(httpClient.www.text);
                        // string resultId = responsePacket.id;

                        m_requestList[idx] = null;
                    }
                    else if (Time.time - httpClient.startTime >= m_option.timeout / 1000f)
                    {
                        m_requestList[idx].Dispose();
                        m_requestList[idx] = null;
                    }
                }
            }
        }

        /// <summary>
        /// Capture a message and send it to sentry server.
        /// </summary>
        /// <param name="message"></param>
        /// <param name="logType"></param>
        /// <param name="tags"></param>
        /// <param name="stackTrace"></param>
		public void CaptureMessage(string message, LogType logType = LogType.Error, Dictionary<string, string> tags = null, string stackTrace = null)
        {
            if (m_valid)
            {
                MessagePacket packet = new MessagePacket(message, logType, tags, stackTrace);

                Send(packet.ToJson());
            }
        }

        /// <summary>
        /// Capture a message with System.Diagnostics.StackTrace
        /// </summary>
        /// <param name="message"></param>
        /// <param name="logType"></param>
        /// <param name="tags"></param>
        /// <param name="stackTrace"></param>
        public void CaptureMessageWithSysStack(string message, LogType logType = LogType.Error, Dictionary<string, string> tags = null, System.Diagnostics.StackTrace stackTrace = null)
        {
            if (m_valid)
            {
                MessagePacket packet = new MessagePacket(message, logType, tags, stackTrace);

                Send(packet.ToJson());
            }
        }

        /// <summary>
        /// Send the captured exception to sentry server.
        /// </summary>
        /// <param name="exception">Captured exception</param>
        /// <param name="tags"></param>
		public void CaptureException(Exception exception, Dictionary<string, string> tags = null)
        {
            CaptureException(exception.Message, new System.Diagnostics.StackTrace(exception, true), tags);
        }

        /// <summary>
        /// Send an exception message with stack trace to sentry server.
        /// </summary>
        /// <param name="message">The description of the captured exception</param>
        /// <param name="stackTrace">The stack trace of the captured exception</param>
        /// <param name="tags"></param>
        public void CaptureException(string message, string stackTrace, Dictionary<string, string> tags = null)
        {
            if (m_valid)
            {
                ExceptionPacket packet = new ExceptionPacket(message, stackTrace, tags);

                Send(packet.ToJson());
            }
        }

        /// <summary>
        /// Send an exception message with stack trace to sentry server.
        /// </summary>
        /// <param name="message">The description of the captured exception</param>
        /// <param name="stackTrace">The stack trace of the captured exception</param>
        /// <param name="tags"></param>
        public void CaptureException(string message, System.Diagnostics.StackTrace stackTrace, Dictionary<string, string> tags = null)
        {
            if (m_valid)
            {
                ExceptionPacket packet = new ExceptionPacket(message, stackTrace, tags);

                Send(packet.ToJson());
            }
        }

        private void Send(string payload)
        {
            int count = 0;
            int idx = -1;
            while (count++ < m_requestList.Count)
            {
                m_lastIdx++;

                if (m_lastIdx >= m_requestList.Count)
                {
                    m_lastIdx = 0;
                }

                if (m_requestList[m_lastIdx] == null)
                {
                    idx = m_lastIdx;
                    break;
                }
            }

            if (idx >= 0)
            {
                m_requestList[idx] = new HttpClient(m_dsn, payload, m_option);
                StartCoroutine(m_requestList[idx].AsyncSend());
            }
        }
    }

    class HttpClient
    {
        public WWW www { get; private set; }
        private DSN m_dsn;
        private string m_payload;
        private RavenOptionType m_option;
        public float startTime { get; private set; }

        public HttpClient(DSN dsn, string payload, RavenOptionType option)
        {
            m_dsn = dsn;
            m_payload = payload;
            m_option = option;
        }

        public IEnumerator AsyncSend()
        {
            Dictionary<string, string> headers = new Dictionary<string, string>();
            headers.Add("Accept", "application/json");
            headers.Add("Content-Type", "application/json; charset=utf-8");
            headers.Add("X-Sentry-Auth", m_dsn.XSentryAuthHeader());
            headers.Add("User-Agent", m_dsn.UserAgent());

            if (m_option.compression)
            {
                headers.Add("Content-Encoding", "gzip");
            }

            byte[] requestBuffer = null;
            using (MemoryStream requestStream = new MemoryStream())
            {
                if (m_option.compression)
                {
                    byte[] payloadBuffer = System.Text.Encoding.UTF8.GetBytes(m_payload);
                    using (GZipOutputStream gzipStream = new GZipOutputStream(requestStream))
                    {
                        gzipStream.Write(payloadBuffer, 0, payloadBuffer.Length);
                    }
                }
                else
                {
                    using (StreamWriter streamWriter = new StreamWriter(requestStream))
                    {
                        streamWriter.Write(m_payload);
                    }
                }
                requestBuffer = requestStream.ToArray();
            }

            startTime = Time.time;
            www = new WWW(m_dsn.sentryUri, requestBuffer, headers);
            yield return www;
        }

        public void Dispose()
        {
            www.Dispose();
        }
    }
}