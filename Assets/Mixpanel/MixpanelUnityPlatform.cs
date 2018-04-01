using UnityEngine;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace mixpanel.platform
{
    public class MixpanelUnityPlatform
    {
        #if UNITY_ANDROID && !UNITY_EDITOR
        public static string get_android_advertising_id()
        {
            try
            {
                AndroidJavaClass up = new AndroidJavaClass("com.unity3d.player.UnityPlayer");
                AndroidJavaObject currentActivity = up.GetStatic<AndroidJavaObject>("currentActivity");
                AndroidJavaClass client = new AndroidJavaClass("com.google.android.gms.ads.identifier.AdvertisingIdClient");
                AndroidJavaObject adInfo = client.CallStatic<AndroidJavaObject>("getAdvertisingIdInfo",currentActivity);
                return adInfo.Call<string>("getId").ToString(); // note: we're not using the id for advertising
            }
            catch (AndroidJavaException e)
            {
                return null;
            }
        }

        public static string get_android_id()
        {
            AndroidJavaClass clsUnity = new AndroidJavaClass("com.unity3d.player.UnityPlayer");
            AndroidJavaObject objActivity = clsUnity.GetStatic<AndroidJavaObject>("currentActivity");
            AndroidJavaObject objResolver = objActivity.Call<AndroidJavaObject>("getContentResolver");
            AndroidJavaClass clsSecure = new AndroidJavaClass("android.provider.Settings$Secure");
            string android_id = clsSecure.CallStatic<string>("getString", objResolver, "android_id");
            return android_id;
        }

        //string versionName = context().getPackageManager().getPackageInfo(context().getPackageName(), 0).versionName;
        public static string get_android_version_name()
        {
            AndroidJavaClass contextCls = new AndroidJavaClass("com.unity3d.player.UnityPlayer");
            AndroidJavaObject context = contextCls.GetStatic<AndroidJavaObject>("currentActivity");
            AndroidJavaObject packageMngr = context.Call<AndroidJavaObject>("getPackageManager");
            string packageName = context.Call<string>("getPackageName");
            AndroidJavaObject packageInfo = packageMngr.Call<AndroidJavaObject>("getPackageInfo", packageName, 0);
            return packageInfo.Get<string>("versionName");
        }

        //int vesioncode = context().getPackageManager().getPackageInfo(context().getPackageName(), 0).versionCode;
        public static int get_android_version_code()
        {
            AndroidJavaClass contextCls = new AndroidJavaClass("com.unity3d.player.UnityPlayer");
            AndroidJavaObject context = contextCls.GetStatic<AndroidJavaObject>("currentActivity");
            AndroidJavaObject packageMngr = context.Call<AndroidJavaObject>("getPackageManager");
            string packageName = context.Call<string>("getPackageName");
            AndroidJavaObject packageInfo = packageMngr.Call<AndroidJavaObject>("getPackageInfo", packageName, 0);
            return packageInfo.Get<int>("versionCode");
        }
        #endif

        public static string get_distinct_id()
        {
            #if UNITY_ANDROID && !UNITY_EDITOR
            string result = get_android_advertising_id();
            if (string.IsNullOrEmpty(result))
            {
                Debug.Log("Android Advertising ID not available, using ANDROID_ID");
                return get_android_id();
            }
            return result;
            #else
            return SystemInfo.deviceUniqueIdentifier;
            #endif
        }

        public static string get_storage_directory()
        {
            return Application.persistentDataPath;
        }
    }
}
