using UnityEngine;
using UnityEngine.UI;

namespace Crosstales.UI.Util
{
    /// <summary>Simple FPS-Counter.</summary>
    public class FPSDisplay : MonoBehaviour
    {

        #region Variables

        /// <summary>Text component to display the FPS.</summary>
        [Tooltip("Text component to display the FPS.")]
        public Text FPS;

        private float deltaTime = 0f;
        private float elapsedTime = 0f;

        private float msec;
        private float fps;

        private const string wait = "<i>...calculating <b>FPS</b>...</i>";
        private const string red = "<color=#E57373><b>FPS: {0:0.}</b> ({1:0.0} ms)</color>";
        private const string orange = "<color=#FFB74D><b>FPS: {0:0.}</b> ({1:0.0} ms)</color>";
        private const string green = "<color=#81C784><b>FPS: {0:0.}</b> ({1:0.0} ms)</color>";

        #endregion


        #region MonoBehaviour methods

        public void Update()
        {
            deltaTime += (Time.deltaTime - deltaTime) * 0.1f;
            elapsedTime += Time.deltaTime;

            if (elapsedTime > 1f)
            {
                if (Time.frameCount % 3 == 0 && FPS != null)
                {
                    msec = deltaTime * 1000f;
                    fps = 1f / deltaTime;

                    if (fps < 15)
                    {
                        FPS.text = string.Format(red, fps, msec);
                    }
                    else if (fps < 29)
                    {
                        FPS.text = string.Format(orange, fps, msec);
                    }
                    else
                    {
                        FPS.text = string.Format(green, fps, msec);
                    }
                }
            }
            else
            {
                FPS.text = wait;
            }
        }

        #endregion
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)