using UnityEngine;
using System.Collections;
using System;
using System.Reflection;

namespace VoxelBusters.Utility
{
	public static class MonoBehaviourExtensions
	{
		#region Properties

		private static bool		isPaused	= false;
		private static float	timeScale	= 1f;

		#endregion

		#region Pause Resume Methods

		public static void PauseUnity (this MonoBehaviour _monoTarget)
		{
			// Pause only if its not in paused state
			if (!isPaused)
			{
				Debug.LogWarning("[MonoBehaviourExtensions] Paused");
				isPaused		= true;

				// Cache timescale, later used for resetting
				timeScale		= Time.timeScale;
				Time.timeScale	= 0f;
			}
		}

		public static void ResumeUnity (this MonoBehaviour _monoTarget)
		{
			// Resume only if paused
			if (isPaused)
			{
				Debug.LogWarning("[MonoBehaviourExtensions] Resumed");
				isPaused		= false;
				
				// Reset timescale
				Time.timeScale	= timeScale;
			}
		}

		#endregion
	}
}