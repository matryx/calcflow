using UnityEngine;
using System.Collections;
using System.Collections.Generic;

namespace VoxelBusters.Utility
{
	public static class TextureExtensions
	{
		public enum EncodeTo
		{
			JPG,
			PNG
		}

		#region Serialization Methods
		
		public static string ToString (this Texture2D _img, EncodeTo _encodeTo = EncodeTo.PNG)
		{
			// Converting texture data to string
			byte[] _bytes		= (_encodeTo == EncodeTo.JPG) ? _img.EncodeToJPG() : _img.EncodeToPNG();
			string _strTexData	= System.Convert.ToBase64String(_bytes);

			return _strTexData;
		}
		
		public static string Serialise (this Texture2D _img, EncodeTo _encodeTo = EncodeTo.PNG)
		{
			// Need to append width and height information
			IDictionary _dict	= new Dictionary<string, object>();
			_dict["width"]		= _img.width;
			_dict["height"]		= _img.height;
			_dict["texture"]	= _img.ToString(_encodeTo);
			
			// Json format string
			string _strJson		= JSONUtility.ToJSON(_dict);
			return _strJson;
		}

		public static Texture2D Deserialise (string _strImg)
		{
			IDictionary _decodedDict	= JSONUtility.FromJSON(_strImg) as Dictionary<string, object>;

			if (_decodedDict != null)
			{
				// Extracting properties from padded fields
				int _width				= (int)_decodedDict["width"];
				int _height				= (int)_decodedDict["height"];
				string _texDataB64		= _decodedDict["texture"] as string;

				return CreateTexture(_texDataB64, _width, _height);  
			}

			return null;
		}

		public static Texture2D CreateTexture (string _texDataB64, int _width, int _height)
		{
			// Loading texture from byte array
			byte[] _bytes			= System.Convert.FromBase64String(_texDataB64);
			Texture2D _newTexture	= new Texture2D(_width, _height);
			_newTexture.LoadImage(_bytes);
			
			return _newTexture;
		}

		#endregion

		#region Screenshot Methods

		public static IEnumerator TakeScreenshot (System.Action<Texture2D> _onCompletionHandler)
		{
			Texture2D _texture;
			
			yield return new WaitForEndOfFrame();
			
			// Read pixel
			_texture	= new Texture2D(Screen.width, Screen.height);
			_texture.ReadPixels(new Rect(0f, 0f, Screen.width, Screen.height),0,0);
			_texture.Apply();
		
			//Fire the callback if exists
			if(_onCompletionHandler != null)
			{
				_onCompletionHandler(_texture);
			}
		}

		#endregion

		#region Rotate Texture Methods

		public static Texture2D Rotate (this Texture2D _inputTex, float _angle)
		{
//			A_______________C
//			|				|
//			|		.		|
//			|	center		|
//			|				|
//			B_______________D

			int _inputTexWidth		= _inputTex.width;
			int _inputTexHeight		= _inputTex.height;

			// Calculate rotated texture dimension			
			int _rotatedTexWidth, _rotatedTexHeight;
			float _clampedAngle		= (_angle - 45f) % 360;

			if (_clampedAngle < 0)
				_clampedAngle	+= 360f;

			int _quadrant			= (int)(_clampedAngle / 90f) + 1;

			if (_quadrant == 1 || _quadrant == 3) 
			{
				_rotatedTexWidth	= _inputTexHeight;
				_rotatedTexHeight	= _inputTexWidth;
			}
			else
			{
				_rotatedTexWidth	= _inputTexWidth;
				_rotatedTexHeight	= _inputTexHeight;
			}

			// Texture midpoints
			Vector2 _inputTexCenter	= new Vector2(_inputTexWidth * 0.5f, _inputTexHeight * 0.5f);

			// Offsetting such that center of texture is at (0, 0)
			Vector2 _x0				= VectorExtensions.Rotate(new Vector2(-_rotatedTexWidth * 0.5f, -_rotatedTexHeight * 0.5f), _angle) + _inputTexCenter;
			Vector2 _dx				= Vector2.right.Rotate(_angle);
			Vector2 _dy				= Vector2.up.Rotate(_angle);
			Vector2 _x1				= _x0;

			// Pixels
			Color32[] _inputTexPixels	= _inputTex.GetPixels32(0);
			Color32[] _rotatedTexPixels	= new Color32[_inputTexPixels.Length];

			for (int _yCord	= 0; _yCord	< _rotatedTexHeight; _yCord++)
			{
				Vector2 _x2		= _x1;
				int _destIndex	= _yCord * _rotatedTexWidth;

				for (int _xCord	= 0; _xCord < _rotatedTexWidth; _xCord++)
				{
					// Set new pixel based on rotated coordinates
					_rotatedTexPixels[_destIndex++]	= _inputTex.GetPixel(_inputTexPixels, _x2);	

					// Update horizontal coordinates
					_x2	+= _dx;
				}

				// Update vertical coordinates
				_x1 += _dy;
			}

			// Create rotated texture
			Texture2D _rotatedTex	= new Texture2D(_rotatedTexWidth, 	_rotatedTexHeight, 
			                                      _inputTex.format, 	false);
			_rotatedTex.SetPixels32(_rotatedTexPixels, 0);
			_rotatedTex.Apply();

			return _rotatedTex;
		}

		private static Color GetPixel (this Texture2D _inputTex, Color32[] _pixels, Vector2 _coordinate)
		{
			int _xCord	= (int)_coordinate.x;
			int _yCord	= (int)_coordinate.y;

			if (_xCord >= _inputTex.width || _xCord < 0)
				return Color.clear;
			
			if (_yCord >= _inputTex.height || _yCord < 0)
				return Color.clear;

			return _pixels[(_yCord * _inputTex.width) + _xCord];
		}
		
		#endregion

		#region Mirror Textures Method

		public static Texture2D MirrorTexture (this Texture2D _inputTex, bool _mirrorHorizontally, 
		                                       bool _mirrorVertically) 
		{
			int _texWidth				= _inputTex.width;
			int _texWidthMinus1			= _texWidth - 1;
			int _texHeight				= _inputTex.height;
			int _texHeightMinus1		= _texHeight - 1;

			// Pixels
			Color32[] _inputTexPixels	= _inputTex.GetPixels32(0);
			Color32[] _mirrorTexPixels	= new Color32[_inputTexPixels.Length];

			for (int _yCord	= 0; _yCord	< _texHeight; _yCord++)
			{
				int _sourceIndex	= _yCord * _texWidth;
				int _destYCord		= _mirrorVertically ? (_texHeightMinus1 - _yCord) : _yCord;
				int _destIndex		= _destYCord * _texWidth;

				for (int _xCord	= 0; _xCord < _texWidth; _xCord++)
				{
					int _destXCord		= _mirrorHorizontally ? (_texWidthMinus1 - _xCord) : _xCord;

					_mirrorTexPixels[_destIndex + _destXCord]	= _inputTexPixels[_sourceIndex++];
				}
			}

			// Create mirrored texture
			Texture2D _mirrorTex	= new Texture2D(_texWidth, 		_texHeight, 
			                                     _inputTex.format, false);
			_mirrorTex.SetPixels32(_mirrorTexPixels, 0);
			_mirrorTex.Apply();

			Debug.Log("[TextureExtensions:Mirror] Output W=" + _mirrorTex.width + " H=" + _mirrorTex.height);
			return _mirrorTex;
		}

		#endregion

		#region Scale Operation

		public static Texture2D Scale (this Texture2D _inputTex, float _scaleFactor) 
		{
			if (_scaleFactor == 1f)
			{
				return _inputTex;
			}
			else if (_scaleFactor == 0f)
			{
				return Texture2D.blackTexture;
			}

			int _newWidth	=	Mathf.RoundToInt(_inputTex.width 	* _scaleFactor);
			int _newHeight	=	Mathf.RoundToInt(_inputTex.height 	* _scaleFactor);
			

			// Create Color Buffer
			Color[] _scaledTexPixels	= new Color[_newWidth * _newHeight];
	
			for (int _yCord	= 0; _yCord	< _newHeight; _yCord++)
			{
				float _vCord 			= _yCord / (_newHeight - 1f);
				int   _scanLineIndex	= _yCord * _newWidth;
				
				for (int _xCord	= 0; _xCord	< _newWidth; _xCord++)
				{
					float _uCord = _xCord / (_newWidth - 1f);
					
					_scaledTexPixels[_scanLineIndex + _xCord] = _inputTex.GetPixelBilinear(_uCord, _vCord);
				}
			}

			// Create Scaled Texture
			Texture2D _scaledTex	= new Texture2D(_newWidth,	_newHeight,	_inputTex.format, false);
			_scaledTex.SetPixels(_scaledTexPixels, 0);
			_scaledTex.Apply();

			return _scaledTex;
		}

		#endregion
	}
}