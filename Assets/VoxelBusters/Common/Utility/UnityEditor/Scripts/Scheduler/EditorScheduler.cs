using UnityEngine;
using System.Collections;

#if UNITY_EDITOR
using UnityEditor;
#endif

namespace VoxelBusters.Utility
{
#if UNITY_EDITOR
	[InitializeOnLoad]
#endif
	public class EditorScheduler 
	{
		#region Delegates

		public delegate void CallbackFunction ();

		#endregion

		#region Events

		public static event CallbackFunction 	ScheduleUpdate;

		#endregion

		#region Constructor

		static EditorScheduler ()
		{
#if UNITY_EDITOR
			EditorApplication.update	-= Update;
			EditorApplication.update	+= Update;
#endif
		}

		#endregion

		#region Scheduler Methods

		private static void Update ()
		{
#if !NETFX_CORE
			if (ScheduleUpdate == null)
				return;

			System.Delegate[] _scheduledUpdates	= ScheduleUpdate.GetInvocationList();
			int _totalCount						= _scheduledUpdates.Length;

			for (int _iter = 0; _iter < _totalCount; _iter++)
			{
				if (_scheduledUpdates[_iter] != null)
				{
					_scheduledUpdates[_iter].Method.Invoke(null, null);
				}
			}
#endif
		}

		#endregion
	}
}
