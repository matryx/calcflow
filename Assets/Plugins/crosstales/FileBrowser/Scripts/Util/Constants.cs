namespace Crosstales.FB.Util
{
    /// <summary>Collected constants of very general utility for the asset.</summary>
    public abstract class Constants : Common.Util.BaseConstants
    {

        #region Constant variables

        /// <summary>Is PRO-version?</summary>
        public static readonly bool isPro = false;

        /// <summary>Name of the asset.</summary>
        public const string ASSET_NAME = "File Browser";
        //public const string ASSET_NAME = "File Browser PRO";

        /// <summary>Short name of the asset.</summary>
        public const string ASSET_NAME_SHORT = "FB";
        //public const string ASSET_NAME_SHORT = "FB PRO";

        /// <summary>Version of the asset.</summary>
        public const string ASSET_VERSION = "2019.2.2";

        /// <summary>Build number of the asset.</summary>
        public const int ASSET_BUILD = 20190326;

        /// <summary>Create date of the asset (YYYY, MM, DD).</summary>
        public static readonly System.DateTime ASSET_CREATED = new System.DateTime(2017, 8, 1);

        /// <summary>Change date of the asset (YYYY, MM, DD).</summary>
        public static readonly System.DateTime ASSET_CHANGED = new System.DateTime(2019, 3, 26);

        /// <summary>URL of the PRO asset in UAS.</summary>
        public const string ASSET_PRO_URL = "https://www.assetstore.unity3d.com/#!/content/98713?aid=1011lNGT&pubref=" + ASSET_NAME;

        /// <summary>URL for update-checks of the asset</summary>
        public const string ASSET_UPDATE_CHECK_URL = "https://www.crosstales.com/media/assets/fb_versions.txt";
        //public const string ASSET_UPDATE_CHECK_URL = "https://www.crosstales.com/media/assets/test/fb_versions_test.txt";

        /// <summary>Contact to the owner of the asset.</summary>
        public const string ASSET_CONTACT = "fb@crosstales.com";

        /// <summary>URL of the asset manual.</summary>
        public const string ASSET_MANUAL_URL = "https://www.crosstales.com/media/data/assets/FileBrowser/FileBrowser-doc.pdf";

        /// <summary>URL of the asset API.</summary>
        public const string ASSET_API_URL = "https://www.crosstales.com/media/data/assets/FileBrowser/api/";

        /// <summary>URL of the asset forum.</summary>
        public const string ASSET_FORUM_URL = "https://forum.unity.com/threads/file-browser-native-file-browser-for-standalone.510403/";

        /// <summary>URL of the asset in crosstales.</summary>
        public const string ASSET_WEB_URL = "https://www.crosstales.com/en/portfolio/FileBrowser/";

        /// <summary>URL of the promotion video of the asset (Youtube).</summary>
        //public const string ASSET_VIDEO_PROMO = "TBD"; //TODO set correct URL

        /// <summary>URL of the tutorial video of the asset (Youtube).</summary>
        //public const string ASSET_VIDEO_TUTORIAL = "TBD"; //TODO set correct URL

        // Keys for the configuration of the asset
        public const string KEY_PREFIX = "FILEBROWSER_CFG_";
        public const string KEY_DEBUG = KEY_PREFIX + "DEBUG";
        public const string KEY_NATIVE_WINDOWS = KEY_PREFIX + "NATIVE_WINDOWS";

        // Default values
        public const bool DEFAULT_NATIVE_WINDOWS = true;

        #endregion


        #region Changable variables

        // Text fragments for the asset
        public static string TEXT_OPEN_FILE = "Open file";
        public static string TEXT_OPEN_FILES = "Open files";
        public static string TEXT_OPEN_FOLDER = "Open folder";
        public static string TEXT_OPEN_FOLDERS = "Open folders";
        public static string TEXT_SAVE_FILE = "Save file";
        public static string TEXT_ALL_FILES = "All files";
        public static string TEXT_SAVE_FILE_NAME = "MySaveFile";

        #endregion

    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)