using UnityEngine;
using System.Collections;
using System.Collections.Generic;

namespace VoxelBusters.Utility
{
	public struct WebResponse
	{
		#region Properties

		public int			Status
		{
			get;
			private set;
		}

		public string		Message
		{
			get;
			private set;
		}
		
		public IDictionary	Data
		{
			get;
			private set;
		}
		
		public List<string>	Errors
		{
			get;
			private set;
		}

		#endregion

		#region Static Methods

		public static WebResponse WebResponseOnSuccess (IDictionary _jsonResponse)
		{
			WebResponse	_newResponse		= new WebResponse();
			_newResponse.Status				= _jsonResponse.GetIfAvailable<int>("status");
			
			if (_jsonResponse.Contains("response"))
			{
				IDictionary _responseDict	= _jsonResponse["response"] as IDictionary;
				
				if (_responseDict.Contains("data"))
				{
					_newResponse.Data		= _responseDict["data"] as IDictionary;
					_newResponse.Message	= _responseDict.GetIfAvailable<string>("message");
					_newResponse.Errors		= _responseDict.GetIfAvailable<List<string>>("errors");
				}
			}

			return _newResponse;
		}

		public static WebResponse WebResponseOnFail (IDictionary _jsonResponse)
		{
			WebResponse	_newResponse	= new WebResponse();
			_newResponse.Status			= 0;
			_newResponse.Message		= null;
			_newResponse.Data			= null;
			
			// Errors
			_newResponse.Errors	= new List<string>();
			_newResponse.Errors.Add(_jsonResponse["error"] as string);

			return _newResponse;
		}
		
		#endregion
	}
}
