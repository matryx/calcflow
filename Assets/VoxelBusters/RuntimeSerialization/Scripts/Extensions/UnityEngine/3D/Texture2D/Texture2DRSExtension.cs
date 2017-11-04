using UnityEngine;
using System.Collections;
using System;
using VoxelBusters.RuntimeSerialization;
using UnityEditor;

public class Texture2DRSExtension : ObjectRSExtension 
{
	#region Constants
	
	private 	const	string		kWidthKey				= "width";
	private 	const	string		kHeightKey				= "height";
	private 	const	string		kDataKey				= "data";
	private 	const	string		kAnisoLevel				= "anisoLevel";
	private 	const	string		kFilterMode				= "filterMode";
	private 	const	string		kWrapMode				= "wrapMode";
	
	#endregion

	#region Instance Method
	
	public override object CreateInstance (RuntimeSerializationInfo _info)
	{
		int 		_width		= _info.GetValue<int>(kWidthKey,	true);
		int 		_height		= _info.GetValue<int>(kHeightKey,	true);
		
		return new Texture2D(_width, _height);
	}
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Texture2D 	_texture	= _object as Texture2D;

		if (_texture == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_texture, _info);

        // Serializing properties
        try
        {
			_info.AddValue<int>(kWidthKey, 				_texture.width			, true);
			_info.AddValue<int>(kHeightKey,				_texture.height			, true);
			_info.AddValue<byte[]>(kDataKey, 			_texture.EncodeToPNG());
			_info.AddValue<int>(kAnisoLevel, 			_texture.anisoLevel);
			_info.AddValue<FilterMode>(kFilterMode, 	_texture.filterMode);
			_info.AddValue<TextureWrapMode>(kWrapMode, 	_texture.wrapMode);
		}
		catch (UnityException _exception)
		{
			Debug.LogException(_exception);
		}
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Texture2D 		_texture	= base.ReadSerializationData(_object, _info) as Texture2D;

		if (_texture == null)
			return null;

		// Deserialize properties
		_texture.anisoLevel			= _info.GetValue<int>(kAnisoLevel);
		_texture.filterMode			= _info.GetValue<FilterMode>(kFilterMode);
		_texture.wrapMode			= _info.GetValue<TextureWrapMode>(kWrapMode);
		_texture.LoadImage(_info.GetValue<byte[]>(kDataKey));

		return _texture;
	}
	
	#endregion
}