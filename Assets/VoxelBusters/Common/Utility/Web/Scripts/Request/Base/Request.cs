using UnityEngine;
using System.Linq;
using System.Collections;
using System.Collections.Generic;

namespace VoxelBusters.Utility
{
	public abstract class Request 
	{
		#region Properties

		public 			bool 			IsAsynchronous 		 	
		{ 
			get; 
			private set; 
		}

		public 			URL	 			URL  				 	
		{ 
			get; 
			private set; 
		}
		
		protected 		WWW				WWWObject		 	
		{ 
			get; 
			set; 
		}

		private	static	MonoBehaviour	surrogateMonobehaviour;

		#endregion

		#region Constructors

		private Request ()
		{}

		protected Request (URL _URL, bool _isAsynchronous) 
		{
			this.URL				= _URL;
			this.IsAsynchronous		= _isAsynchronous;
		}

		#endregion

		#region Handling Requests

		public void StartRequest ()
		{
			if (WWWObject == null || string.IsNullOrEmpty(URL.URLString))
			{
				Debug.LogError("[WebRequest] Request data is invalid.");
				DidFailStartRequestWithError("The operation could not be completed because request data is invalid.");
				return;
			}

			if (IsAsynchronous)
			{
#if UNITY_EDITOR
				// Coroutine to run in editor mode
				if (!Application.isPlaying)
				{
					EditorCoroutine.StartCoroutine(StartAsynchronousRequest());
					return;
				}
#endif
				// Create surrogate object if required
				if (surrogateMonobehaviour == null)
				{
					GameObject _surrogateGO	= new GameObject();
					
					// Make it persistent object
					GameObject.DontDestroyOnLoad(_surrogateGO);
					
					// Hide it in hierarchy and add a mono component
					_surrogateGO.hideFlags	= HideFlags.HideInHierarchy;
					surrogateMonobehaviour	= _surrogateGO.AddComponent<MonoBehaviour>();
				}

				// Start coroutine using surrogate object
				surrogateMonobehaviour.StartCoroutine(StartAsynchronousRequest());
			}
			else
			{
				while (!WWWObject.isDone)
				{}

				OnFetchingResponse();
			}
		}

		private IEnumerator StartAsynchronousRequest ()
		{
			while (!WWWObject.isDone)
				yield return null;

			OnFetchingResponse();
		}
		
		public void Abort ()
		{
			if (WWWObject != null && !WWWObject.isDone)
				WWWObject.Dispose();
		}
		
		#endregion

		#region Handling Response 

		protected abstract void OnFetchingResponse ();

		protected abstract void DidFailStartRequestWithError (string _error);

		#endregion
	}
}