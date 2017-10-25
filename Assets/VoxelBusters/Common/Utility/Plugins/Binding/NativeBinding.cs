using UnityEngine;
using System.Collections;

#if UNITY_IOS
using System.Runtime.InteropServices;
#endif

namespace VoxelBusters.Utility
{
	public partial class NativeBinding : MonoBehaviour 
	{
		#region Native Methods

#if UNITY_IOS
		[DllImport("__Internal")]
		private static extern string utilityBundleVersion ();

		[DllImport("__Internal")]
		private static extern string utilityBundleIdentifier ();
#endif

		#endregion

		#region Static Methods

		public static string GetBundleVersion ()
		{
#if UNITY_EDITOR
			return null;
#elif UNITY_ANDROID
			return PluginNativeBinding.CallStatic<string>(NativeInfo.Methods.GET_BUILD_VERSION);
#elif UNITY_IOS
			return utilityBundleVersion();
#else
			return null;
#endif
		}

		public static string GetBundleIdentifier ()
		{
#if UNITY_EDITOR
			return null;
#elif UNITY_ANDROID
			return PluginNativeBinding.CallStatic<string>(NativeInfo.Methods.GET_BUILD_IDENTIFIER);
#elif UNITY_IOS
			return utilityBundleIdentifier();
#else
			return null;
#endif
		}

		#endregion
	}
}