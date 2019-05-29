namespace Crosstales.FB.Util
{
    /// <summary>Configuration for the asset.</summary>
    public static class Config
    {

        #region Changable variables

        /// <summary>Enable or disable debug logging for the asset.</summary>
        public static bool DEBUG = Constants.DEFAULT_DEBUG;

        /// <summary>Enable or disable native file browser inside the Unity Editor.</summary>
        public static bool NATIVE_WINDOWS = Constants.DEFAULT_NATIVE_WINDOWS;

        /// <summary>Is the configuration loaded?</summary>
        public static bool isLoaded = false;

        #endregion


        #region Public static methods

        /// <summary>Resets all changable variables to their default value.</summary>
        public static void Reset()
        {
            if (!Constants.DEV_DEBUG)
                DEBUG = Constants.DEFAULT_DEBUG;

            NATIVE_WINDOWS = Constants.DEFAULT_NATIVE_WINDOWS;
        }

        /// <summary>Loads the all changable variables.</summary>
        public static void Load()
        {
            if (!Constants.DEV_DEBUG)
            {
                if (Common.Util.CTPlayerPrefs.HasKey(Constants.KEY_DEBUG))
                {
                    DEBUG = Common.Util.CTPlayerPrefs.GetBool(Constants.KEY_DEBUG);
                }
            }
            else
            {
                DEBUG = Constants.DEV_DEBUG;
            }

            if (Common.Util.CTPlayerPrefs.HasKey(Constants.KEY_NATIVE_WINDOWS))
            {
                NATIVE_WINDOWS = Common.Util.CTPlayerPrefs.GetBool(Constants.KEY_NATIVE_WINDOWS);
            }

            isLoaded = true;
        }

        /// <summary>Saves the all changable variables.</summary>
        public static void Save()
        {
            if (!Constants.DEV_DEBUG)
                Common.Util.CTPlayerPrefs.SetBool(Constants.KEY_DEBUG, DEBUG);

            Common.Util.CTPlayerPrefs.SetBool(Constants.KEY_NATIVE_WINDOWS, NATIVE_WINDOWS);

            Common.Util.CTPlayerPrefs.Save();
        }

        #endregion
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)