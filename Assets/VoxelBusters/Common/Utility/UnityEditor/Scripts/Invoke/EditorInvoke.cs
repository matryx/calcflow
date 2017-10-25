using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

#if UNITY_EDITOR
using UnityEditor;
#endif

namespace VoxelBusters.Utility
{
#if UNITY_EDITOR
	[InitializeOnLoad]
#endif
	public class EditorInvoke 
	{
		#region Properties

#pragma warning disable
		// Related to invoke
		private static double				m_cachedTimeSinceStartup	= 0f;
#pragma warning restore

		private static Dictionary<System.Action, Dictionary<string, float>>	invokeMethodsContainer;

		#endregion

		#region Constants
		
		private const string 				kTimeSinceLastInvoke		= "time-since-last-invoke";
		private const string				kRepeatRate					= "repeat-rate";
		private const string 				kInvokeAfter				= "invoke-after";

		#endregion

		#region Constructors

		static EditorInvoke ()
		{
			invokeMethodsContainer		= new Dictionary<System.Action, Dictionary<string, float>>();

#if UNITY_EDITOR
			m_cachedTimeSinceStartup	= EditorApplication.timeSinceStartup;

			// Register for updates
			EditorApplication.update	-= ManageMethodInvoke;
			EditorApplication.update	+= ManageMethodInvoke;
#endif
		}
	
		#endregion

		#region Invoke Methods

#if UNITY_EDITOR

		private static void ManageMethodInvoke ()
		{
			float _dt					= (float)(EditorApplication.timeSinceStartup - m_cachedTimeSinceStartup);
			System.Action[] _methodList	= invokeMethodsContainer.Keys.ToArray<System.Action>();

			for (int _iter = 0; _iter < _methodList.Length; _iter++)
			{
				System.Action _invokeMethod						= _methodList[_iter];
				Dictionary<string, float> _invokeMethodDetails	= invokeMethodsContainer[_invokeMethod];

				// Time since last invoke is updated
				_invokeMethodDetails[kTimeSinceLastInvoke] 		+= _dt;
				
				// Checking if its time to invoke method
				if (_invokeMethodDetails[kTimeSinceLastInvoke] > _invokeMethodDetails[kInvokeAfter])
				{
					// Invoke method
					_invokeMethod();

					// Invalidate
					if (!_invokeMethodDetails.ContainsKey(kRepeatRate))
					{
						invokeMethodsContainer.Remove(_invokeMethod);
						continue;
					}

					// Reset value
					_invokeMethodDetails[kTimeSinceLastInvoke]	= 0f;
					_invokeMethodDetails[kInvokeAfter]			= _invokeMethodDetails[kRepeatRate];
				}
			}
			
			// Cache time
			m_cachedTimeSinceStartup		= EditorApplication.timeSinceStartup;
		}

#endif

		public static void Invoke (System.Action _method, float _time)
		{
			invokeMethodsContainer[_method]	= new Dictionary<string, float>() 
			{
				{ kTimeSinceLastInvoke, 0f },
				{ kInvokeAfter, _time }
			};
		}

		public static void InvokeRepeating (System.Action _method, float _time, float _repeatRate)
		{
			invokeMethodsContainer[_method]	= new Dictionary<string, float>() 
			{
				{ kTimeSinceLastInvoke, 0f },
				{ kInvokeAfter, _time },
				{ kRepeatRate, _repeatRate }
			};
		}
		
		#endregion
	}
}