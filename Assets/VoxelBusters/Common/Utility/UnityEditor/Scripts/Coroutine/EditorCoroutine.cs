using UnityEngine;
using System.Collections;
using System.Collections.Generic;

#if UNITY_EDITOR
using UnityEditor;
#endif

namespace VoxelBusters.Utility
{
	public class EditorCoroutine  
	{
		#region Properties
		
		public IEnumerator		CoroutineMethod 
		{ 
			get; 
			private set; 
		}

#pragma warning disable
		private bool			m_finishedCoroutine;
#pragma warning restore

		#endregion

		#region Constructor

		private EditorCoroutine ()
		{}

		private EditorCoroutine (IEnumerator _enumerator)
		{
			CoroutineMethod		= _enumerator;
			m_finishedCoroutine	= false;
		}

		#endregion

		#region Exposed Methods

		/// <summary>
		/// Starts editor coroutine.
		/// </summary>
		public static EditorCoroutine StartCoroutine (IEnumerator _enumerator)
		{
			EditorCoroutine _editorCoroutine	= null;

#if UNITY_EDITOR
			_editorCoroutine					= new EditorCoroutine(_enumerator);

			// Starts coroutine
			EditorApplication.update			+= _editorCoroutine.Update;
#endif

			return _editorCoroutine;
		}

		/// <summary>
		/// Stops editor coroutine.
		/// </summary>
		public void StopCoroutine ()
		{			
#if UNITY_EDITOR
			if (!m_finishedCoroutine)
			{
				m_finishedCoroutine				= true;

				// Cancel scheduled method
				EditorApplication.update		-= Update;
			}
#endif
		}

		private void Update ()
		{
			if (!CoroutineMethod.MoveNext())
			{
				StopCoroutine();
			}
		}

		#endregion
	}
}