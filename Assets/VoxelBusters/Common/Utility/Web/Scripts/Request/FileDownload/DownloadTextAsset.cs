using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using ExifLibrary;

namespace VoxelBusters.Utility
{
	public class DownloadTextAsset : Request 
	{
		#region Delegates

		public delegate void Completion (string _text, string _error);

		#endregion

		#region Properties

		public Completion OnCompletion
		{
			get;
			set;
		}

		#endregion

		#region Constructors
		
		public DownloadTextAsset (URL _URL, bool _isAsynchronous, bool _autoFixOrientation) : base(_URL, _isAsynchronous)
		{
			WWWObject	= new WWW(_URL.URLString);
		}
		
		#endregion
		
		#region Handling Response

		protected override void DidFailStartRequestWithError (string _error)
		{
			if (OnCompletion != null)
				OnCompletion(null, _error);
		}

		protected override void OnFetchingResponse ()
		{			
			Debug.Log("[DownloadTextAsset] Did finish downloading, Error=" + WWWObject.error);

			if (string.IsNullOrEmpty(WWWObject.error))
			{
				if (OnCompletion != null)
					OnCompletion(WWWObject.text, null);
			}
			else
			{
				if (OnCompletion != null)
					OnCompletion(null, WWWObject.error);
			}
		}

		#endregion
	}
}
