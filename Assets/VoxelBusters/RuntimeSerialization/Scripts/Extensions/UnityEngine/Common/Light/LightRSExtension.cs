using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class LightRSExtension : BehaviourRSExtension 
{
	#region Constants
	
	private 	const	string		kAlreadyLightmappedKey		= "alreadyLightmapped";
	private 	const	string		kColorKey					= "color";
	private 	const	string		kCullingMaskKey				= "cullingMask";
	private 	const	string		kIntensityKey				= "intensity";
	private 	const	string		kRangeKey					= "range";
	private 	const	string		kRenderModeKey				= "renderMode";
	private 	const	string		kShadowBiasKey				= "shadowBias";
	private 	const	string		kShadowsKey					= "shadows";
	private 	const	string		kShadowStrengthKey			= "shadowStrength";
	private 	const	string		kSpotAngleKey				= "spotAngle";
	private 	const	string		kTypeKey					= "type";

#if UNITY_4_6 || UNITY_4_7
	private 	const	string		kShadowSoftnessKey			= "shadowSoftness";
	private 	const	string		kShadowSoftnessFadeKey		= "shadowSoftnessFade";
#elif UNITY_5
	private 	const	string		kBounceIntensityKey			= "bounceIntensity";
	private 	const	string		kCommandBufferCountKey		= "commandBufferCount";
	private 	const	string		kShadowNormalBiasKey		= "shadowNormalBias";
#endif

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Light		_light		= _object as Light;
		
		if (_light == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serializing properties
		_info.AddValue<bool>(kAlreadyLightmappedKey,	_light.alreadyLightmapped);
		_info.AddValue<Color>(kColorKey, 				_light.color);
		_info.AddValue<int>(kCullingMaskKey,			_light.cullingMask);
		_info.AddValue<float>(kIntensityKey,			_light.intensity);
		_info.AddValue<float>(kRangeKey, 				_light.range);
		_info.AddValue<LightRenderMode>(kRenderModeKey, _light.renderMode);
		_info.AddValue<float>(kShadowBiasKey,			_light.shadowBias);
		_info.AddValue<LightShadows>(kShadowsKey,		_light.shadows);
		_info.AddValue<float>(kShadowStrengthKey, 		_light.shadowStrength);
		_info.AddValue<float>(kSpotAngleKey, 			_light.spotAngle);
		_info.AddValue<LightType>(kTypeKey, 			_light.type);

#if UNITY_4_6 || UNITY_4_7
		_info.AddValue<float>(kShadowSoftnessKey, 		_light.shadowSoftness);
		_info.AddValue<float>(kShadowSoftnessFadeKey, 	_light.shadowSoftnessFade);
#elif UNITY_5
		_info.AddValue<float>(kBounceIntensityKey, 		_light.bounceIntensity);
		_info.AddValue<float>(kShadowNormalBiasKey, 	_light.shadowNormalBias);
#endif
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Light		_light			= base.ReadSerializationData(_object, _info) as Light;
		
		if (_light == null)
			return null;
		
		// Deserialize properties
		_light.alreadyLightmapped	= _info.GetValue<bool>(kAlreadyLightmappedKey);
		_light.color				= _info.GetValue<Color>(kColorKey);
		_light.cullingMask			= _info.GetValue<int>(kCullingMaskKey);
		_light.intensity			= _info.GetValue<float>(kIntensityKey);
		_light.range				= _info.GetValue<float>(kRangeKey);
		_light.renderMode			= _info.GetValue<LightRenderMode>(kRenderModeKey);
		_light.shadowBias			= _info.GetValue<float>(kShadowBiasKey);
		_light.shadows				= _info.GetValue<LightShadows>(kShadowsKey);
		_light.shadowStrength		= _info.GetValue<float>(kShadowStrengthKey);
		_light.spotAngle			= _info.GetValue<float>(kSpotAngleKey);
		_light.type					= _info.GetValue<LightType>(kTypeKey);

#if UNITY_4_6 || UNITY_4_7
		_light.shadowSoftness		= _info.GetValue<float>(kShadowSoftnessKey);
		_light.shadowSoftnessFade	= _info.GetValue<float>(kShadowSoftnessFadeKey);
#elif UNITY_5
		_light.bounceIntensity		= _info.GetValue<float>(kBounceIntensityKey);
		_light.shadowNormalBias		= _info.GetValue<float>(kShadowNormalBiasKey);
#endif
		
		return _light;
	}
	
	#endregion
}