using UnityEngine;
using System.Linq;
using System.Collections;
using System.Collections.Generic;

namespace VoxelBusters.Utility
{
	public abstract class WebRequest : Request
	{
		#region Delegates

		public delegate void JSONResponse (IDictionary _response);

		#endregion

		#region Properties
		
		public object 			Parameters  		 	
		{ 
			get; 
			set; 
		}

		public JSONResponse 	OnSuccess  	 	
		{ 
			get; 
			set; 
		}

		public JSONResponse		OnFailure  	 	
		{ 
			get; 
			set; 
		}

		#endregion

		#region Constructors
		
		public WebRequest (URL _URL, object _params, bool _isAsynchronous) : base(_URL, _isAsynchronous)
		{
			this.Parameters			= _params;
		}

		#endregion

		#region Handling Requests
		
		protected abstract WWW CreateWWWObject ();

		#endregion

		#region Handling Response 

		protected override void DidFailStartRequestWithError (string _error)
		{
			IDictionary _responseDict = new Dictionary<string, string>(){
				{ "error", _error }
			};
			
			if (OnFailure != null)
				OnFailure(_responseDict);
		}

		protected override void OnFetchingResponse ()
		{
			// Create respone based on completion data
			if (string.IsNullOrEmpty(WWWObject.error))
			{
				IDictionary _responseDict = JSONUtility.FromJSON(WWWObject.text) as IDictionary;

				if (OnSuccess != null)
					OnSuccess(_responseDict);
			}
			else
			{
				IDictionary _responseDict = new Dictionary<string, string>(){
					{ "error", WWWObject.error }
				};

				if (OnFailure != null)
					OnFailure(_responseDict);
			}
		}

		#endregion
	}
}