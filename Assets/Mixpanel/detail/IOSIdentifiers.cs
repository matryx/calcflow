using System.Runtime.InteropServices;

namespace mixpanel.detail {
    public class IOSIdentifiers
    {
        #if UNITY_IOS && !UNITY_EDITOR && MIXPANEL_USE_IOS_IFA
        [DllImport("__Internal")] public static extern string mixpanel_ios_get_idfa();
        #endif

        #if UNITY_IOS && MIXPANEL_USE_IOS_IFA
        public static readonly bool MIXPANEL_USE_IOS_IFA_ENABLED = true;
        #else
        public static readonly bool MIXPANEL_USE_IOS_IFA_ENABLED = false;
        #endif
    }
}
