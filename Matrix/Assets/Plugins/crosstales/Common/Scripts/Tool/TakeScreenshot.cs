using UnityEngine;
using System;

namespace Crosstales.Common.Util
{
    /// <summary>Take screen shots inside an application.</summary>
    [DisallowMultipleComponent]
    public class TakeScreenshot : MonoBehaviour
    {

        #region Variables

        ///<summary>Prefix for the generate file names.</summary>
        [Tooltip("Prefix for the generate file names.")]
        public string Prefix = "CT_Screenshot";

        ///<summary>Factor by which to increase resolution (default: 1).</summary>
        [Tooltip("Factor by which to increase resolution (default: 1).")]
        public int Scale = 1;

        ///<summary>Key-press to capture the screen (default: F8).</summary>
        [Tooltip("Key-press to capture the screen (default: F8).")]
        public KeyCode KeyCode = KeyCode.F8;

        private Texture2D texture;

        #endregion

#if (!UNITY_WSA && !UNITY_WEBGL) || UNITY_EDITOR
        #region MonoBehaviour methods

        public void Start()
        {
            DontDestroyOnLoad(transform.root.gameObject);
        }

        public void Update()
        {
            if (Input.GetKeyDown(KeyCode))
            {
                Capture();
            }
        }

        #endregion


        #region Public methods

        ///<summary>Capture the screen.</summary>
        public void Capture()
        {
            string file = Prefix + DateTime.Now.ToString("_dd-MM-yyyy-HH-mm-ss-f") + ".png";

#if UNITY_2017_1_OR_NEWER
                ScreenCapture.CaptureScreenshot(file, Scale);
#else
            Application.CaptureScreenshot(file, Scale);
#endif

            Debug.Log("Screenshot saved: " + file);
        }

        #endregion
#else
        public void Start()
        {
            Debug.LogWarning("'TakeScreenshot' doesn't work with the current platform!");
        }
#endif
    }
}
// © 2014-2019 crosstales LLC (https://www.crosstales.com)