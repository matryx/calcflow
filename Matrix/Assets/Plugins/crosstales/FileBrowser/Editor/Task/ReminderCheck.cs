using UnityEngine;
using UnityEditor;
using Crosstales.FB.EditorUtil;

namespace Crosstales.FB.EditorTask
{
    /// <summary>Reminds the customer to create an UAS review.</summary>
    [InitializeOnLoad]
    public static class ReminderCheck
    {
        #region Constructor

        static ReminderCheck()
        {
            string lastDate = EditorPrefs.GetString(EditorConstants.KEY_REMINDER_DATE);
            string date = System.DateTime.Now.ToString("yyyyMMdd"); // every day
            //string date = System.DateTime.Now.ToString("yyyyMMddHHmm"); // every minute (for tests)

            if (!date.Equals(lastDate))
            {
                int count = EditorPrefs.GetInt(EditorConstants.KEY_REMINDER_COUNT) + 1;

                //if (count % 1 == 0) // for testing only
                if (count % 13 == 0 && EditorConfig.REMINDER_CHECK)
                {

                    int option = EditorUtility.DisplayDialogComplex(Util.Constants.ASSET_NAME + " - Reminder",
                                "Please don't forget to rate " + Util.Constants.ASSET_NAME + " or even better write a little review – it would be very much appreciated!",
                                "Yes, let's do it!",
                                "Not right now",
                                "Don't ask again!");

                    if (option == 0)
                    {
                        Application.OpenURL(EditorConstants.ASSET_URL);
                        EditorConfig.REMINDER_CHECK = false;

                        Debug.LogWarning("<color=red>" + Common.Util.BaseHelper.CreateString("❤", 500) + "</color>");
                        Debug.LogWarning("<b>+++ Thank you for rating <color=blue>" + Util.Constants.ASSET_NAME + "</color>! +++</b>");
                        Debug.LogWarning("<color=red>" + Common.Util.BaseHelper.CreateString("❤", 500) + "</color>");
                    }
                    else if (option == 1)
                    {
                        // do nothing!
                    }
                    else
                    {
                        EditorConfig.REMINDER_CHECK = false;
                    }

                    EditorConfig.Save();
                }

                EditorPrefs.SetString(EditorConstants.KEY_REMINDER_DATE, date);
                EditorPrefs.SetInt(EditorConstants.KEY_REMINDER_COUNT, count);
            }
        }

        #endregion

    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)