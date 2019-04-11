using UnityEngine;

namespace Crosstales.Common.Util
{
    /// <summary>Enables or disable game objects for a given platform.</summary>
    //[HelpURL("https://www.crosstales.com/media/data/assets/radio/api/class_crosstales_1_1_radio_1_1_demo_1_1_util_1_1_platform_controller.html")]
    public class PlatformController : MonoBehaviour
    {

        #region Variables

        [Header("Configuration")]

        ///<summary>Selected platforms for the controller.</summary>
        [Tooltip("Selected platforms for the controller.")]
        public System.Collections.Generic.List<Model.Enum.Platform> Platforms;

        ///<summary>Enable or disable the 'Objects' for the selected 'Platforms' (default: true).</summary>
        [Tooltip("Enable or disable the 'Objects' for the selected 'Platforms' (default: true).")]
        public bool Active = true;


        [Header("Objects")]
        ///<summary>Selected objects for the controller.</summary>
        [Tooltip("Selected objects for the controller.")]
        public GameObject[] Objects;

        protected Model.Enum.Platform currentPlatform;

        #endregion


        #region MonoBehaviour methods

        public virtual void Start()
        {
            selectPlatform();
        }

        #endregion


        #region Private methods

        protected void selectPlatform()
        {
            currentPlatform = BaseHelper.CurrentPlatform;

            activateGO();
        }

        protected void activateGO()
        {
            bool active = Platforms.Contains(currentPlatform) ? Active : !Active;

            foreach (GameObject go in Objects)
            {
                if (go != null)
                {
                    go.SetActive(active);
                }
            }
        }

        #endregion
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)