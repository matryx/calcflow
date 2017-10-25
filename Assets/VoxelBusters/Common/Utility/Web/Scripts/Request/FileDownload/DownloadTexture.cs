using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using ExifLibrary;

namespace VoxelBusters.Utility
{
	public class DownloadTexture : Request 
	{
		#region Delegates

		public delegate void Completion (Texture2D _texture, string _error);

		#endregion

		#region Properties

		public bool			AutoFixOrientation 
		{ 
			get; 
			set; 
		}

		public float ScaleFactor 
		{
			get;
			set;
		}

		public Completion	OnCompletion 
		{ 
			get; 
			set; 
		}

		#endregion

		#region Constructors

		public DownloadTexture (URL _URL, bool _isAsynchronous, bool _autoFixOrientation) : base(_URL, _isAsynchronous)
		{
			AutoFixOrientation	= _autoFixOrientation;
			WWWObject			= new WWW(_URL.URLString);
			ScaleFactor			= 1f;
		}

		#endregion
		
		#region Overriden Methods
		
		protected override void DidFailStartRequestWithError (string _error)
		{
			if (OnCompletion != null)
				OnCompletion(null, _error);
		}

		protected override void OnFetchingResponse ()
		{
			Texture2D _finalTexture	= null;

			if (!string.IsNullOrEmpty(WWWObject.error))
			{
				Debug.Log("[DownloadTexture] Failed to download texture. Error = " + WWWObject.error + ".");

				if (OnCompletion != null)
				{
					OnCompletion(null, WWWObject.error);
					return;
				}
			}

			Texture2D _tempTexture	= WWWObject.texture;

			// Fix orientation to normal
		#if !UNITY_WINRT
			if (AutoFixOrientation)
			{
				Stream  _textureStream 	= new MemoryStream(WWWObject.bytes);	

				ExifFile _exifFile 		= ExifFile.Read(_textureStream);

				// Scale texture first before rotating for performance.
				_tempTexture	=	_tempTexture.Scale(ScaleFactor);
				
				if(_exifFile != null && _exifFile.Properties.ContainsKey(ExifTag.Orientation))
				{
					Orientation _orientation	= (Orientation)_exifFile.Properties[ExifTag.Orientation].Value;
					Debug.Log("[DownloadTexture] Orientation=" + _orientation);

					switch (_orientation)
					{
					case Orientation.Normal:
						// Original image is used
						_finalTexture	= _tempTexture;
						break;

					case Orientation.MirroredVertically:
						// Invert horizontally
						_finalTexture	= _tempTexture.MirrorTexture(true, false);
						break;

					case Orientation.Rotated180:
						// Invert horizontally as well as vertically 
						_finalTexture	= _tempTexture.MirrorTexture(true, true);
						break;

					case Orientation.MirroredHorizontally:
						// Invert vertically 
						_finalTexture	= _tempTexture.MirrorTexture(false, true);
						break;

					case Orientation.RotatedLeftAndMirroredVertically:
						// Invert horizontally and rotate by -90
						_finalTexture	= _tempTexture.MirrorTexture(true, false).Rotate(-90);
						break;

					case Orientation.RotatedRight:
						// Rotate by 90
						_finalTexture	= _tempTexture.Rotate(90);
						break;

					case Orientation.RotatedLeft:
						// Invert vertically and rotate by -90
						_finalTexture	= _tempTexture.MirrorTexture(false, true).Rotate(-90);
						break;

					case Orientation.RotatedRightAndMirroredVertically:
						// Rotate by -90
						_finalTexture	= _tempTexture.Rotate(-90);
						break;
					}
					
				}
				else
				{
					_finalTexture	= _tempTexture;
				}
			}
			// Use original image 
			else
		#endif
			{
				_tempTexture	= _tempTexture.Scale(ScaleFactor);
				_finalTexture	= _tempTexture;
			}

			if (OnCompletion != null)
				OnCompletion(_finalTexture, null);
		}

		#endregion
	}
}
