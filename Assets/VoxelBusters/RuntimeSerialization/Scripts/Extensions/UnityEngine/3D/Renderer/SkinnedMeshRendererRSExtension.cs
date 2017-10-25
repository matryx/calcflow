using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class SkinnedMeshRendererRSExtension : RendererRSExtension 
{
	#region Constants
	
	private 	const	string		kBonesKey				= "bones";
	private 	const	string		kLocalBoundsKey			= "localBounds";
	private 	const	string		kQualityKey				= "quality";
	private 	const	string		kSharedMeshKey			= "sharedMesh";
	private 	const	string		kUpdateWhenOffscreenKey	= "updateWhenOffscreen";

#if UNITY_4_6
	private 	const	string		kRootBoneKey			= "rootBone";
#endif

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SkinnedMeshRenderer	_renderer	= _object as SkinnedMeshRenderer;
		
		if (_renderer == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<Transform[]>(kBonesKey, 			_renderer.bones);
		_info.AddValue<Bounds>(kLocalBoundsKey,			_renderer.localBounds);
		_info.AddValue<SkinQuality>(kQualityKey,		_renderer.quality);
		_info.AddValue<Mesh>(kSharedMeshKey,			_renderer.sharedMesh);
		_info.AddValue<bool>(kUpdateWhenOffscreenKey,	_renderer.updateWhenOffscreen);

#if UNITY_4_6
		_info.AddValue<Transform>(kRootBoneKey,			_renderer.rootBone);
#endif
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		// Deserialize base properties
		SkinnedMeshRenderer	_renderer	= base.ReadSerializationData(_object, _info) as SkinnedMeshRenderer;
		
		if (_renderer == null)
			return null;
		
		// Deserialize properties
		_renderer.bones					= _info.GetValue<Transform[]>(kBonesKey);
		_renderer.localBounds			= _info.GetValue<Bounds>(kLocalBoundsKey);
		_renderer.quality				= _info.GetValue<SkinQuality>(kQualityKey);
		_renderer.sharedMesh			= _info.GetValue<Mesh>(kSharedMeshKey);
		_renderer.updateWhenOffscreen	= _info.GetValue<bool>(kUpdateWhenOffscreenKey);

#if UNITY_4_6
		_renderer.rootBone				= _info.GetValue<Transform>(kRootBoneKey);
#endif
		
		return _renderer;
	}
	
	#endregion
}
