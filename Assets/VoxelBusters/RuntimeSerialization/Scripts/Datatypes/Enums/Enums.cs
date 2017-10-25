using UnityEngine;
using System.Collections;

namespace VoxelBusters.RuntimeSerialization
{
	/// <summary>
	/// Saves serialization data to specified save target.
	/// </summary>
	public enum eSaveTarget : byte
	{
#if (UNITY_WEBPLAYER || UNITY_WEBGL)
		/// <summary>
		/// Saves serialization data to PlayerPrefs.
		/// </summary>
		PLAYER_PREFS
#else
		/// <summary>
		/// Saves serialization data to PlayerPrefs.
		/// </summary>
		PLAYER_PREFS,
		/// <summary>
		/// Saves serialization data to files. This option is not available for Unity WebPlayer.
		/// </summary>
		FILE_SYSTEM
#endif
	}
}