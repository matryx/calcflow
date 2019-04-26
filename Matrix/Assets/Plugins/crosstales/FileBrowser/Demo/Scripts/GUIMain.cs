using UnityEngine;
using UnityEngine.UI;
using UnityEngine.SceneManagement;
using Crosstales.FB.Util;

namespace Crosstales.FB.Demo
{
    /// <summary>Main GUI component for all demo scenes.</summary>
    [HelpURL("https://www.crosstales.com/media/data/assets/FileBrowser/api/class_crosstales_1_1_f_b_1_1_demo_1_1_g_u_i_main.html")]
    public class GUIMain : MonoBehaviour
    {

        #region Variables

        public Text Name;
        public Text Version;
        public Text Scene;

        #endregion


        #region MonoBehaviour methods

        public void Start()
        {
            if (Name != null)
            {
                Name.text = Constants.ASSET_NAME;
            }

            if (Version != null)
            {
                Version.text = Constants.ASSET_VERSION;
            }

            if (Scene != null)
            {
                Scene.text = SceneManager.GetActiveScene().name;
            }
        }

        #endregion


        #region Public methods

        public void OpenAssetURL()
        {
            Application.OpenURL(Constants.ASSET_CT_URL);
        }

        public void OpenCTURL()
        {
            Application.OpenURL(Constants.ASSET_AUTHOR_URL);
        }

        public void Quit()
        {
            if (Application.isEditor)
            {
#if UNITY_EDITOR
                UnityEditor.EditorApplication.isPlaying = false;
#endif
            }
            else
            {
                Application.Quit();
            }
        }

        #endregion
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)