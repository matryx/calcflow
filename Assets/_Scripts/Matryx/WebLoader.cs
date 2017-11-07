using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Web
{
    public delegate void WebDelegate(string result);

    public class WebLoader : MonoBehaviour
    {
        private static WebLoader _instance;
        public static WebLoader Instance
        {
            get
            {
                if (_instance == null)
                {
                    _instance = new GameObject().AddComponent<WebLoader>();
                }

                return _instance;
            }
        }

        private IEnumerator LoadFromURL(string url, WebDelegate webDelegate)
        {
            WWW www = new WWW(url);
            yield return www;
            if (!String.IsNullOrEmpty(www.text))
            {
                webDelegate(www.text);
            }
            else
            {
                Debug.Log("ERROR: " + www.error);
            }
        }

        public void Load(string url, WebDelegate webDelegate)
        {
            StartCoroutine(LoadFromURL(url, webDelegate));
        }
    }
}
