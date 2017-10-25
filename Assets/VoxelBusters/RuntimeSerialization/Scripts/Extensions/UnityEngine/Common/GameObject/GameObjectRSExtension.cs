using UnityEngine;
using System.Collections;
using System;
using VoxelBusters.RuntimeSerialization;
using VoxelBusters.RuntimeSerialization.Internal;

public class GameObjectRSExtension : ObjectRSExtension
{
	#region Constants
	
	private 	const	string		kActiveSelfKey			= "activeSelf";
	private 	const	string		kIsStaticKey			= "isStatic";
	private 	const	string		kLayerKey				= "layer";
	private 	const	string		kTagKey					= "tag";
	private 	const	string		kNameKey				= "name";
	private 	const	string		kTransformKey			= "transform";
	private 	const	string		kComponentsKey			= "components";
	private		const 	string		kHierarchyMetadataKey	= "metadata";
	private		const 	string		kIsPrefabKey			= "isPrefab";

	#endregion
	
	#region Instance Method
	
	public override object CreateInstance (RuntimeSerializationInfo _info)
	{
		string		_hierarchyMetadata	= _info.GetValue<string>(kHierarchyMetadataKey, true);
		bool		_activeSelf			= _info.GetValue<bool>(kActiveSelfKey, true);
		bool		_isPrefab			= _info.GetValue<bool>(kIsPrefabKey, true);
		GameObject	_gameObject			= UnityObjectSerializationUtil.GetGameObject(_hierarchyMetadata, _isPrefab);

#if RS_DEBUG_MODE
		Debug.Log(string.Format("[RS] Finished creating gameobject instance with hierarchy metadata: {0}.", _hierarchyMetadata), _gameObject);
#endif
			
		// Set active state
		_gameObject.SetActive(_activeSelf);

		return _gameObject;
	}
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		GameObject 	_gameObject			= _object as GameObject;
		
		if (_gameObject == null)
			return;

		UIDSystem	_UIDSystem			= _gameObject.GetComponent<UIDSystem>();

		if (_UIDSystem == null)
		{
			Debug.LogError(string.Format("[RS] The operation could not be completed because UIDSystem component is found missing in gameobject with name: {0}.", _gameObject.name), _gameObject);
			return;
		}

		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serializing properties
		string			_hierarchyMetadata	= UnityObjectSerializationUtil.GetHierarchyMetadata(_gameObject);
		Component[] 	_components			= _UIDSystem.GetSerializableComponents();
		bool			_activeSelf			= _gameObject.activeSelf;
		bool			_isPrefab			= _UIDSystem.IsPrefab;

		// Initializer properties
		_info.AddValue<string>(kHierarchyMetadataKey, _hierarchyMetadata, 	true);
		_info.AddValue<bool>(kActiveSelfKey, 		_activeSelf, 			true);
		_info.AddValue<bool>(kIsPrefabKey, 			_isPrefab, 				true);

		// Other properties
		_info.AddValue<bool>(kIsStaticKey, 			_gameObject.isStatic);
		_info.AddValue<int>(kLayerKey, 				_gameObject.layer);
		_info.AddValue<string>(kTagKey, 			_gameObject.tag);
		_info.AddValue<string>(kNameKey, 			_gameObject.name);
		_info.AddValue<Component[]>(kComponentsKey, _components);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		GameObject 	_gameObject		= base.ReadSerializationData(_object, _info) as GameObject;

		if (_gameObject == null)
			return null;

		// Deserialize properties
		_gameObject.SetActive(_info.GetValue<bool>(kActiveSelfKey, true));
		_gameObject.isStatic		= _info.GetValue<bool>(kIsStaticKey);
		_gameObject.layer			= _info.GetValue<int>(kLayerKey);
		_gameObject.tag				= _info.GetValue<string>(kTagKey);
		_gameObject.name			= _info.GetValue<string>(kNameKey);

		return _gameObject;
	}
	
	#endregion
}