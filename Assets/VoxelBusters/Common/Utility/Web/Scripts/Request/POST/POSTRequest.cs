using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public class POSTRequest : WebRequest 
	{
		#region Constructors
		
		public POSTRequest (URL _URL, object _params, bool _isAsynchronous) : base(_URL, _params, _isAsynchronous)
		{
			WWWObject	= CreateWWWObject();
		}
		
		#endregion
		
		#region Overriden Methods

		protected override WWW CreateWWWObject ()
		{
			string _serverURL	= URL.URLString;

			// NULL parameter handling
			if (Parameters == null)
			{
				Debug.LogWarning("[POSTRequest] Post data is missing");
				return new WWW(_serverURL);
			}

			// POST request needs us to send post data
			IDictionary _paramDict	= (Parameters as IDictionary);
			if (_paramDict == null)
			{
				Debug.LogError("[POSTRequest] Invalid parameter");
				return null;
			}

			// Return WWW with post data
			string _jsonStr		= _paramDict.ToJSON();
			byte[] _postData	= System.Text.Encoding.UTF8.GetBytes(_jsonStr);

			return new WWW(_serverURL, _postData);
		}

		#endregion

		#region Static Methods
		
		public static POSTRequest CreateRequest (URL _URL, object _params)
		{
			POSTRequest _request	= new POSTRequest(_URL, _params, false); 
			
			return _request;
		}
		
		public static POSTRequest CreateAsyncRequest (URL _URL, object _params)
		{
			POSTRequest _request	= new POSTRequest(_URL, _params, false); 
			
			return _request;
		}
		
		#endregion
	}
}
