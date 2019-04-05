namespace Crosstales.FB.Util
{
    /// <summary>Various helper functions.</summary>
    public abstract class Helper : Common.Util.BaseHelper
    {

        #region Static properties

        /// <summary>Checks if the current platform is supported.</summary>
        /// <returns>True if the current platform is supported.</returns>
        public static bool isSupportedPlatform
        {
            get
            {
                if (Constants.isPro)
                {
                    return isWindowsPlatform || isMacOSPlatform || isLinuxPlatform || isWSAPlatform;
                }

                return isWindowsPlatform || isMacOSPlatform || isLinuxPlatform;
            }
        }

        #endregion
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)