using UnityEngine;
using UnityEditor;
using Crosstales.FB.EditorUtil;

namespace Crosstales.FB.EditorTask
{
    /// <summary>Checks for updates of the asset.</summary>
    [InitializeOnLoad]
    public static class UpdateCheck
    {
        #region Variables

        public const string TEXT_NOT_CHECKED = "Not checked.";
        public const string TEXT_NO_UPDATE = "No update available - you are using the latest version.";

        private static UpdateStatus status = UpdateStatus.NOT_CHECKED;

        private static readonly char[] splitChar = new char[] { ';' };

        #endregion


        #region Constructor

        static UpdateCheck()
        {
            if (EditorConfig.UPDATE_CHECK)
            {
                if (Util.Config.DEBUG)
                    Debug.Log("Updater enabled!");

                string lastDate = EditorPrefs.GetString(EditorConstants.KEY_UPDATE_DATE);
                string date = System.DateTime.Now.ToString("yyyyMMdd"); // every day
                //string date = System.DateTime.Now.ToString("yyyyMMddHHmm"); // every minute (for tests)

                if (Common.Util.BaseConstants.DEV_DEBUG)
                    Debug.Log("Last check: " + lastDate);

                if (!date.Equals(lastDate))
                {
                    if (Common.Util.BaseHelper.isInternetAvailable)
                    {
                        if (Util.Config.DEBUG)
                            Debug.Log("Checking for update...");

                        //new System.Threading.Thread(() => updateCheck()).Start();
                        updateCheck();

                        EditorPrefs.SetString(EditorConstants.KEY_UPDATE_DATE, date);
                    }
                    else
                    {
                        if (Util.Config.DEBUG)
                            Debug.Log("No Internet available!");
                    }
                }
                else
                {
                    if (Util.Config.DEBUG)
                        Debug.Log("No update check needed.");
                }
            }
            else
            {
                if (Util.Config.DEBUG)
                    Debug.Log("Updater disabled!");
            }
        }

        #endregion


        #region Static methods

        public static void UpdateCheckForEditor(out string result, out UpdateStatus st)
        {
            string[] data = readData();

            updateStatus(data);

            if (status == UpdateStatus.UPDATE)
            {
                result = updateTextForEditor(data);
            }
            else if (status == UpdateStatus.UPDATE_PRO)
            {
                result = updateProTextForEditor(data);
            }
            else if (status == UpdateStatus.UPDATE_VERSION)
            {
                result = updateVersionTextForEditor(data);
            }
            else if (status == UpdateStatus.DEPRECATED)
            {
                result = deprecatedTextForEditor(data);
            }
            else
            {
                result = TEXT_NO_UPDATE;
            }

            st = status;
        }

        #endregion


        #region Private methods

        private static void updateCheck()
        {
            string[] data = readData();

            updateStatus(data);

            if (status == UpdateStatus.UPDATE)
            {
                int option = EditorUtility.DisplayDialogComplex(Util.Constants.ASSET_NAME + " - Update available",
                updateText(data),
                "Yes, let's do it!",
                "Not right now",
                "Don't check again!");

                if (option == 0)
                {
                    Application.OpenURL(EditorConstants.ASSET_URL);
                }
                else if (option == 1)
                {
                    // do nothing!
                }
                else
                {
                    EditorConfig.UPDATE_CHECK = false;

                    EditorConfig.Save();
                }
            }
            else if (status == UpdateStatus.UPDATE_PRO)
            {
                int option = EditorUtility.DisplayDialogComplex(Util.Constants.ASSET_NAME + " - Upgrade needed",
                updateProText(data),
                "Yes, let's do it!",
                "Not right now",
                "Don't ask again!");

                if (option == 0)
                {
                    Application.OpenURL(Util.Constants.ASSET_PRO_URL);
                }
                else if (option == 1)
                {
                    // do nothing!
                }
                else
                {
                    EditorConfig.UPDATE_CHECK = false;

                    EditorConfig.Save();
                }
            }
            else if (status == UpdateStatus.UPDATE_VERSION)
            {
                int option = EditorUtility.DisplayDialogComplex(Util.Constants.ASSET_NAME + " - Upgrade needed",
                updateVersionText(data),
                "Yes, let's do it!",
                "Not right now",
                "Don't ask again!");

                if (option == 0)
                {
                    Application.OpenURL(EditorConstants.ASSET_URL);
                }
                else if (option == 1)
                {
                    // do nothing!
                }
                else
                {
                    EditorConfig.UPDATE_CHECK = false;

                    EditorConfig.Save();
                }
            }
            else if (status == UpdateStatus.DEPRECATED)
            {
                int option = EditorUtility.DisplayDialogComplex(Util.Constants.ASSET_NAME + " - Upgrade needed",
                deprecatedText(data),
                "Learn more",
                "Not right now",
                "Don't bother me again!");

                if (option == 0)
                {
                    Application.OpenURL(Common.Util.BaseConstants.ASSET_AUTHOR_URL);
                }
                else if (option == 1)
                {
                    // do nothing!
                }
                else
                {
                    EditorConfig.UPDATE_CHECK = false;

                    EditorConfig.Save();
                }
            }
            else
            {
                if (Util.Config.DEBUG)
                    Debug.Log("Asset is up-to-date.");
            }
        }

        private static string updateText(string[] data)
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();

            if (data != null)
            {
                sb.Append("Your version:\t");
                sb.Append(Util.Constants.ASSET_VERSION);
                sb.Append(System.Environment.NewLine);
                sb.Append("New version:\t");
                sb.Append(data[2]);
                sb.Append(System.Environment.NewLine);
                sb.Append(System.Environment.NewLine);
                sb.AppendLine("Please download the new version from the Unity AssetStore!");
            }

            return sb.ToString();
        }

        private static string updateProText(string[] data)
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();

            if (data != null)
            {
                sb.Append(Util.Constants.ASSET_NAME);
                sb.Append(" is deprecated in favour of the PRO-version!");
                sb.Append(System.Environment.NewLine);
                sb.Append(System.Environment.NewLine);
                sb.AppendLine("Please consider an upgrade in the Unity AssetStore.");
            }

            return sb.ToString();
        }

        private static string updateVersionText(string[] data)
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();

            if (data != null)
            {
                sb.Append(Util.Constants.ASSET_NAME);
                sb.Append(" is deprecated in favour of an newer version!");
                sb.Append(System.Environment.NewLine);
                sb.Append(System.Environment.NewLine);
                sb.AppendLine("Please consider an upgrade in the Unity AssetStore.");
            }

            return sb.ToString();
        }

        private static string deprecatedText(string[] data)
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();

            if (data != null)
            {
                sb.Append(Util.Constants.ASSET_NAME);
                sb.Append(" is deprecated!");
                sb.Append(System.Environment.NewLine);
                sb.Append(System.Environment.NewLine);
                sb.AppendLine("Please check the link for more information:");
                sb.AppendLine(Common.Util.BaseConstants.ASSET_AUTHOR_URL);
            }

            return sb.ToString();
        }

        private static string[] readData()
        {
            string[] data = null;

            try
            {
                System.Net.ServicePointManager.ServerCertificateValidationCallback = Common.Util.BaseHelper.RemoteCertificateValidationCallback;

                using (System.Net.WebClient client = new Common.Util.CTWebClient())
                {
                    string content = client.DownloadString(Util.Constants.ASSET_UPDATE_CHECK_URL);

                    foreach (string line in Common.Util.BaseHelper.SplitStringToLines(content))
                    {
                        if (line.StartsWith(EditorConstants.ASSET_UID.ToString()))
                        {
                            data = line.Split(splitChar, System.StringSplitOptions.RemoveEmptyEntries);

                            if (data != null && data.Length >= 3)
                            { 
                                //valid record?
                                break;
                            }
                            else
                            {
                                data = null;
                            }
                        }
                    }
                }
            }
            catch (System.Exception ex)
            {
                Debug.LogError("Could not load update file: " + System.Environment.NewLine + ex);
            }

            return data;
        }

        private static void updateStatus(string[] data)
        {
            if (data != null)
            {
                int buildNumber;

                if (int.TryParse(data[1], out buildNumber))
                {
                    if (buildNumber > Util.Constants.ASSET_BUILD)
                    {
                        status = UpdateStatus.UPDATE;
                    }
                    else if (buildNumber == -100)
                    {
                        status = UpdateStatus.UPDATE_PRO;
                    }
                    else if (buildNumber == -200)
                    {
                        status = UpdateStatus.UPDATE_VERSION;
                    }
                    else if (buildNumber == -900)
                    {
                        status = UpdateStatus.DEPRECATED;
                    }
                    else
                    {
                        status = UpdateStatus.NO_UPDATE;
                    }
                }
            }
        }

        private static string updateTextForEditor(string[] data)
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();

            if (data != null)
            {
                sb.AppendLine("Update found!");
                sb.Append(System.Environment.NewLine);
                sb.Append("Your version:\t");
                sb.Append(Util.Constants.ASSET_VERSION);
                sb.Append(System.Environment.NewLine);
                sb.Append("New version:\t");
                sb.Append(data[2]);
                sb.Append(System.Environment.NewLine);
                sb.Append(System.Environment.NewLine);
                sb.AppendLine("Please download the new version from the Unity AssetStore.");
            }

            return sb.ToString();
        }

        private static string updateProTextForEditor(string[] data)
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();

            if (data != null)
            {
                sb.Append(Util.Constants.ASSET_NAME);
                sb.Append(" is deprecated in favour of the PRO-version!");
                sb.Append(System.Environment.NewLine);
                sb.Append(System.Environment.NewLine);
                sb.AppendLine("Please consider an upgrade in the Unity AssetStore.");
            }

            return sb.ToString();
        }

        private static string updateVersionTextForEditor(string[] data)
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();

            if (data != null)
            {
                sb.Append(Util.Constants.ASSET_NAME);
                sb.Append(" is deprecated in favour of an newer version!");
                sb.Append(System.Environment.NewLine);
                sb.Append(System.Environment.NewLine);
                sb.AppendLine("Please consider an upgrade in the Unity AssetStore.");
            }

            return sb.ToString();
        }

        private static string deprecatedTextForEditor(string[] data)
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();

            if (data != null)
            {
                sb.Append(Util.Constants.ASSET_NAME);
                sb.Append(" is deprecated!");
                sb.Append(System.Environment.NewLine);
                sb.Append(System.Environment.NewLine);
                sb.AppendLine("Please click below for more information.");
            }

            return sb.ToString();
        }

        #endregion
    }

    /// <summary>All possible update stati.</summary>
    public enum UpdateStatus
    {
        NOT_CHECKED,
        NO_UPDATE,
        UPDATE,
        UPDATE_PRO,
        UPDATE_VERSION,
        DEPRECATED
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)