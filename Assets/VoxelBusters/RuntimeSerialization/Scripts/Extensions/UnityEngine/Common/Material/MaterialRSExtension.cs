using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using VoxelBusters.RuntimeSerialization;
using VoxelBusters.Utility;

public class MaterialRSExtension : ObjectRSExtension
{
	#region Constants
	
	private 	const	string		kMainTextureOffsetKey			= "textureOffset";
	private 	const	string		kMainTextureScaleKey			= "textureScale";
	private 	const	string		kShaderKey						= "shader";

#if UNITY_5
	private 	const	string		kGlobalIlluminationFlagsKey		= "illuminationFlags";
#endif

	#endregion

	#region Instance Method
	
	public override object CreateInstance (RuntimeSerializationInfo _info)
	{
		Shader 		_shader		= _info.GetValue<Shader>(kShaderKey, true);

		return new Material(_shader);
	}
	
	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Material 	_material	= _object as Material;

		if (_material == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serialize properties
		_info.AddValue<Shader>(kShaderKey, 				_material.shader, 				true);
		_info.AddValue<Vector2>(kMainTextureOffsetKey, 	_material.mainTextureOffset);
		_info.AddValue<Vector2>(kMainTextureScaleKey, 	_material.mainTextureScale);

#if UNITY_5
		_info.AddValue<MaterialGlobalIlluminationFlags>(kGlobalIlluminationFlagsKey,	_material.globalIlluminationFlags);
#endif

		ShaderUtility.ShaderInfo				_shaderInfo			= ShaderUtility.Instance.GetShaderInfo(_material);

		if (_shaderInfo == null)
		{
			Debug.LogWarning("[RS] Material's shader properties couldnt be serialized.");
			return;
		}

		List<ShaderUtility.ShaderProperty> 		_shaderPropertyList	= _shaderInfo.PropertyList;
		
		for (int _iter = 0; _iter < _shaderPropertyList.Count; _iter++)
		{
			ShaderUtility.ShaderProperty		_curProperty		= _shaderPropertyList[_iter];
			string								_curPropertyName	= _curProperty.Name;
			ShaderUtility.eShaderPropertyType	_curPropertyType	= _curProperty.Type;

			switch (_curPropertyType)
			{
			case ShaderUtility.eShaderPropertyType.COLOR:
				_info.AddValue<Color>(_curPropertyName, 	_material.GetColor(_curPropertyName));
				break;

			case ShaderUtility.eShaderPropertyType.FLOAT:
			case ShaderUtility.eShaderPropertyType.RANGE:
				_info.AddValue<float>(_curPropertyName, 	_material.GetFloat(_curPropertyName));
				break;

			case ShaderUtility.eShaderPropertyType.VECTOR:
				_info.AddValue<Vector4>(_curPropertyName, 	_material.GetVector(_curPropertyName));
				break;

			case ShaderUtility.eShaderPropertyType.TEXTURE:
				_info.AddValue<Texture2D>(_curPropertyName, _material.GetTexture(_curPropertyName) as Texture2D);
				break;
			}
		}
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		// Deserialize base properties
		Material	_material				= base.ReadSerializationData(_object, _info) as Material;

		if (_material == null)
			return null;

		// Deserialize properties
		_material.shader					= _info.GetValue<Shader>(kShaderKey, true);
		_material.mainTextureOffset			= _info.GetValue<Vector2>(kMainTextureOffsetKey);
		_material.mainTextureScale			= _info.GetValue<Vector2>(kMainTextureScaleKey);

#if UNITY_5
		_material.globalIlluminationFlags	= _info.GetValue<MaterialGlobalIlluminationFlags>(kGlobalIlluminationFlagsKey);
#endif
	
		ShaderUtility.ShaderInfo				_shaderInfo			= ShaderUtility.Instance.GetShaderInfo(_material);
		
		if (_shaderInfo == null)
		{
			Debug.LogWarning("[RS] Material's shader properties couldnt be deserialized.");
			return _material;
		}

		List<ShaderUtility.ShaderProperty> 		_shaderPropertyList	= _shaderInfo.PropertyList;

		for (int _iter = 0; _iter < _shaderPropertyList.Count; _iter++)
		{
			ShaderUtility.ShaderProperty		_curProperty		= _shaderPropertyList[_iter];
			string								_curPropertyName	= _curProperty.Name;
			ShaderUtility.eShaderPropertyType	_curPropertyType	= _curProperty.Type;

			switch (_curPropertyType)
			{
			case ShaderUtility.eShaderPropertyType.COLOR:
				_material.SetColor(_curPropertyName, 	_info.GetValue<Color>(_curPropertyName));
				break;
				
			case ShaderUtility.eShaderPropertyType.FLOAT:
			case ShaderUtility.eShaderPropertyType.RANGE:
				_material.SetFloat(_curPropertyName, 	_info.GetValue<float>(_curPropertyName));
				break;
				
			case ShaderUtility.eShaderPropertyType.VECTOR:
				_material.SetVector(_curPropertyName, 	_info.GetValue<Vector4>(_curPropertyName));
				break;
				
			case ShaderUtility.eShaderPropertyType.TEXTURE:
				_material.SetTexture(_curPropertyName, 	_info.GetValue<Texture2D>(_curPropertyName));
				break;
			}
		}

		return _material;
	}

	#endregion
}