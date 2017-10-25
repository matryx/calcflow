using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public struct URL 
	{
		#region Properties

		public string URLString
		{
			get;
			private set;
		}

		#endregion

		#region Constants

		private const string  kFileProtocol		= "file://";
		private const string  kHttpProtocol		= "http://";
		private const string  kHttpsProtocol	= "https://";

		#endregion

		#region Constructors

		private URL (string _URLString) : this ()
		{
			URLString	= _URLString;
		}

		#endregion

		#region Static Methods

		public static URL FileURLWithPath (string _rootPath, string _relativePath)
		{
			return FileURLWithPath(_rootPath + "/" + _relativePath);
		}

		public static URL FileURLWithPath (string _path)
		{
			string _URLString	= null;
			
			if (!string.IsNullOrEmpty(_path))
			{
				if (!_path.StartsWith(kFileProtocol))
				    _URLString	= kFileProtocol + _path;
				else
					_URLString 	= _path;
			}

			return new URL(_URLString);
		}

		public static URL URLWithString (string _rootURLString, string _relativePath)
		{
			return URLWithString(_rootURLString + "/" + _relativePath);
		}
	
		public static URL URLWithString (string _URLString)
		{
			if(string.IsNullOrEmpty(_URLString))
			{
				return new URL();
			}
			else if (_URLString.StartsWith(kHttpProtocol) || _URLString.StartsWith(kHttpsProtocol))
			{
				return new URL(_URLString);
			}
			else
			{
				_URLString	= kHttpProtocol + _URLString;
				return new URL(_URLString);
			}
		}

		#endregion

		#region Methods

		public bool isFileReferenceURL ()
		{
			return URLString.StartsWith(kFileProtocol);
		}

		public override string ToString ()
		{
			return string.Format("[URL: {0}]", URLString);
		}

		#endregion
	}
}
