using UnityEngine;

namespace Crosstales.UI
{
    /// <summary>Crosstales social media links.</summary>
    public class Social : MonoBehaviour
    {

        #region Public methods

        public void Facebook()
        {
            Application.OpenURL(Common.Util.BaseConstants.ASSET_SOCIAL_FACEBOOK);
        }

        public void Twitter()
        {
            Application.OpenURL(Common.Util.BaseConstants.ASSET_SOCIAL_TWITTER);
        }

        public void LinkedIn()
        {
            Application.OpenURL(Common.Util.BaseConstants.ASSET_SOCIAL_LINKEDIN);
        }

        public void Xing()
        {
            Application.OpenURL(Common.Util.BaseConstants.ASSET_SOCIAL_XING);
        }

        public void Youtube()
        {
            Application.OpenURL(Common.Util.BaseConstants.ASSET_SOCIAL_YOUTUBE);
        }

        public void Discord()
        {
            Application.OpenURL(Common.Util.BaseConstants.ASSET_SOCIAL_DISCORD);
        }

        #endregion
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)