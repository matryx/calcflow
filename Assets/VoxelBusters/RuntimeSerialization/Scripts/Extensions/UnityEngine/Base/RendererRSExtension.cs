using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

#if UNITY_5 || UNITY_6
using UnityEngine.Rendering;
#endif

public class RendererRSExtension : ComponentRSExtension 
{
	#region Constants
	
	private 	const	string		kEnabledKey						= "enabled";
	private 	const	string		kLightmapIndexKey				= "lightmapIndex";
	private 	const	string		kMaterialsKey					= "materials";
	private 	const	string		kReceiveShadowsKey				= "receiveShadows";
	private 	const	string		kSharedMaterialsKey				= "sharedMaterials";
	private 	const	string		kSortingLayerIDKey 				= "sortingLayerID";
	private 	const	string		kSortingLayerNameKey 			= "sortingLayerName";
	private 	const	string		kSortingOrderKey 				= "sortingOrder";
	private 	const	string		kUseLightProbesKey				= "useLightProbes";

#if UNITY_4_6 || UNITY_4_7
	private 	const	string		kCastShadowsKey					= "castShadows";
	private 	const	string		kLightmapTilingOffsetKey		= "lightmapTilingOffset";
	private 	const	string		kLightProbeAnchorKey			= "lightProbeAnchor";
#elif UNITY_5
	private 	const	string		kProbeAnchorKey					= "probeAnchor";
	private 	const	string		kLightmapScaleOffsetKey			= "lightmapScaleOffset";
	private 	const	string		kRealtimeLightmapScaleOffsetKey	= "realtimeLightmapScaleOffset";
	private 	const	string		kReflectionProbeUsageKey		= "reflectionProbeUsage";
	private 	const	string		kShadowCastingModeKey			= "shadowCastingMode";
#endif

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Renderer		_renderer			= _object as Renderer;

		if (_renderer == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		Material[] 		_sharedMaterials	= _renderer.sharedMaterials;
		
		if (_sharedMaterials != null)
			_info.AddValue<Material[]>(kSharedMaterialsKey, _sharedMaterials);
		else 
			_info.AddValue<Material[]>(kMaterialsKey, 		_renderer.materials);

		_info.AddValue<bool>(kEnabledKey, 					_renderer.enabled);
		_info.AddValue<int>(kLightmapIndexKey,				_renderer.lightmapIndex);
		_info.AddValue<bool>(kReceiveShadowsKey, 			_renderer.receiveShadows);
		_info.AddValue<int>(kSortingLayerIDKey, 			_renderer.sortingLayerID);
		_info.AddValue<string>(kSortingLayerNameKey, 		_renderer.sortingLayerName);
		_info.AddValue<int>(kSortingOrderKey, 				_renderer.sortingOrder);
		_info.AddValue<bool>(kUseLightProbesKey, 			_renderer.useLightProbes);

#if UNITY_4_6 || UNITY_4_7
		_info.AddValue<bool>(kCastShadowsKey, 				_renderer.castShadows);
		_info.AddValue<Vector4>(kLightmapTilingOffsetKey, 	_renderer.lightmapTilingOffset);
		_info.AddValue<Transform>(kLightProbeAnchorKey, 	_renderer.lightProbeAnchor);
#elif UNITY_5
		_info.AddValue<Transform>(kProbeAnchorKey, 						_renderer.probeAnchor);
		_info.AddValue<Vector4>(kLightmapScaleOffsetKey, 				_renderer.lightmapScaleOffset);
		_info.AddValue<Vector2>(kRealtimeLightmapScaleOffsetKey,		_renderer.realtimeLightmapScaleOffset);
		_info.AddValue<ReflectionProbeUsage>(kReflectionProbeUsageKey, 	_renderer.reflectionProbeUsage);
		_info.AddValue<ShadowCastingMode>(kShadowCastingModeKey, 		_renderer.shadowCastingMode);
#endif
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		// Deserialize base properties
		Renderer		_renderer				= base.ReadSerializationData(_object, _info) as Renderer;

		if (_renderer == null)
			return null;

		// Deserialize properties
		Material[]		_sharedMaterials		= _info.GetValue<Material[]>(kSharedMaterialsKey);
		
		if (_sharedMaterials != null)
			_renderer.sharedMaterials			= _sharedMaterials;
		else 
			_renderer.materials					= _info.GetValue<Material[]>(kMaterialsKey);

		_renderer.enabled						= _info.GetValue<bool>(kEnabledKey);
		_renderer.lightmapIndex					= _info.GetValue<int>(kLightmapIndexKey);
		_renderer.receiveShadows				= _info.GetValue<bool>(kReceiveShadowsKey);
		_renderer.sortingLayerID				= _info.GetValue<int>(kSortingLayerIDKey);
		_renderer.sortingLayerName				= _info.GetValue<string>(kSortingLayerNameKey);
		_renderer.sortingOrder					= _info.GetValue<int>(kSortingOrderKey);
		_renderer.useLightProbes				= _info.GetValue<bool>(kUseLightProbesKey);

#if UNITY_4_6 || UNITY_4_7
		_renderer.castShadows					= _info.GetValue<bool>(kCastShadowsKey);
		_renderer.lightmapTilingOffset			= _info.GetValue<Vector4>(kLightmapTilingOffsetKey);
		_renderer.lightProbeAnchor				= _info.GetValue<Transform>(kLightProbeAnchorKey);
#elif UNITY_5
		_renderer.probeAnchor					= _info.GetValue<Transform>(kProbeAnchorKey);
		_renderer.lightmapScaleOffset			= _info.GetValue<Vector4>(kLightmapScaleOffsetKey);
		_renderer.realtimeLightmapScaleOffset	= _info.GetValue<Vector2>(kRealtimeLightmapScaleOffsetKey);
		_renderer.reflectionProbeUsage			= _info.GetValue<ReflectionProbeUsage>(kReflectionProbeUsageKey);
		_renderer.shadowCastingMode				= _info.GetValue<ShadowCastingMode>(kShadowCastingModeKey);
#endif

		return _renderer;
	}
	
	#endregion
}