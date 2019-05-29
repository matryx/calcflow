namespace Crosstales.FB.EditorUtil
{
    /// <summary>Collected editor constants of very general utility for the asset.</summary>
    public static class EditorConstants
    {

        #region Constant variables

        public const string KEY_UPDATE_CHECK = Util.Constants.KEY_PREFIX + "UPDATE_CHECK";
        public const string KEY_UPDATE_OPEN_UAS = Util.Constants.KEY_PREFIX + "UPDATE_OPEN_UAS";
        public const string KEY_REMINDER_CHECK = Util.Constants.KEY_PREFIX + "REMINDER_CHECK";
        public const string KEY_CT_REMINDER_CHECK = Util.Constants.KEY_PREFIX + "CT_REMINDER_CHECK";
        public const string KEY_TRACER = Util.Constants.KEY_PREFIX + "TRACER";

        // Keys for the configuration of the asset
        public const string KEY_UPDATE_DATE = Util.Constants.KEY_PREFIX + "UPDATE_DATE";

        public const string KEY_REMINDER_DATE = Util.Constants.KEY_PREFIX + "REMINDER_DATE";
        public const string KEY_REMINDER_COUNT = Util.Constants.KEY_PREFIX + "REMINDER_COUNT";
        
        public const string KEY_CT_REMINDER_DATE = Util.Constants.KEY_PREFIX + "CT_REMINDER_DATE";
        public const string KEY_CT_REMINDER_COUNT = Util.Constants.KEY_PREFIX + "CT_REMINDER_COUNT";
        
        public const string KEY_TRACER_DATE = Util.Constants.KEY_PREFIX + "TRACER_DATE";

        // Default values
        public const string DEFAULT_ASSET_PATH = "/Plugins/crosstales/FileBrowser/";
        public const bool DEFAULT_UPDATE_CHECK = true;
        public const bool DEFAULT_UPDATE_OPEN_UAS = false;
        public const bool DEFAULT_REMINDER_CHECK = true;
        public const bool DEFAULT_CT_REMINDER_CHECK = true;
        public const bool DEFAULT_TRACER = true;

        #endregion


        #region Properties

        /// <summary>Returns the URL of the asset in UAS.</summary>
        /// <returns>The URL of the asset in UAS.</returns>
        public static string ASSET_URL
        {
            get
            {

                if (Util.Constants.isPro)
                {
                    return Util.Constants.ASSET_PRO_URL;
                }
                else
                {
                    return "https://www.assetstore.unity3d.com/#!/content/98716?aid=1011lNGT&pubref=" + Util.Constants.ASSET_NAME;
                }
            }
        }

        /// <summary>Returns the ID of the asset in UAS.</summary>
        /// <returns>The ID of the asset in UAS.</returns>
        public static string ASSET_ID
        {
            get
            {
                if (Util.Constants.isPro)
                {
                    return "98713";
                }
                else
                {
                    return "98716";
                }
            }
        }

        /// <summary>Returns the UID of the asset.</summary>
        /// <returns>The UID of the asset.</returns>
        public static System.Guid ASSET_UID
        {
            get
            {
                if (Util.Constants.isPro)
                {
                    return new System.Guid("f9c139be-4da6-4d0f-894a-0675635af15f");
                }
                else
                {
                    return new System.Guid("62455202-c9ff-4b04-b6c3-1d4b836a5fff");
                }
            }
        }

        #endregion

    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)