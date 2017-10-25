using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public class PlayerSettings : MonoBehaviour
	{
		#region Static Methods

		public static string GetBundleVersion ()
		{
#if UNITY_EDITOR
			return UnityEditor.PlayerSettings.bundleVersion;
#else
			return NativeBinding.GetBundleVersion();
#endif
		}

		public static string GetBundleIdentifier ()
		{
#if UNITY_EDITOR
			return UnityEditor.PlayerSettings.applicationIdentifier;
#else
			return NativeBinding.GetBundleIdentifier();
#endif
		}

		#endregion
	}
}