using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;
using VoxelBusters.RuntimeSerialization.Internal;

public class ComponentRSExtension : ObjectRSExtension
{
	#region Constants

	private		const 	string		kHierarchyMetadataKey	= "metadata";
	private		const 	string		kComponentUIDKey		= "uid";
	private		const 	string		kIsPrefabKey			= "isPrefab";

	#endregion

	#region Instance Method

	public override object CreateInstance (RuntimeSerializationInfo _info)
	{
		string		_hierarchyMetadata	= _info.GetValue<string>(kHierarchyMetadataKey, true);
		string		_componentUID		= _info.GetValue<string>(kComponentUIDKey, 		true);
		bool		_isPrefab			= _info.GetValue<bool>(kIsPrefabKey, 			true);
		System.Type	_componentType		= _info.ObjectType;
		Component	_component			= UnityObjectSerializationUtil.GetComponent(_hierarchyMetadata, _componentUID, _componentType, _isPrefab);

#if RS_DEBUG_MODE
		Debug.Log(string.Format("[RS] Finished creating component of type:{0} having UID: {1} and metadata: {2}.", _componentType, _componentUID, _hierarchyMetadata), _component);
#endif

		return _component;
	}

	#endregion

	#region Serialization Methods

	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Component		_component		= _object as Component;

		if (_component == null)
			return;

		GameObject		_gameObject		= _component.gameObject;
		UIDSystem		_UIDSystem		= _gameObject.GetComponent<UIDSystem>();
		
		if (_UIDSystem == null)
		{
			Debug.LogError(string.Format("[RS] The operation could not be completed because UIDSystem component is found missing in gameobject with name: {0}.", _gameObject.name), _gameObject);
			return;
		}

		// Serialize base properties
		base.WriteSerializationData (_object, _info);

		// Serialize properties
		string		_hierarchyMetadata	= UnityObjectSerializationUtil.GetHierarchyMetadata(_gameObject);
		string		_componentUID		= _UIDSystem.GetComponentUID(_component);

		_info.AddValue<string>(kHierarchyMetadataKey, 	_hierarchyMetadata, 	true);
		_info.AddValue<string>(kComponentUIDKey, 		_componentUID,			true);
		_info.AddValue<bool>(kIsPrefabKey, 				_UIDSystem.IsPrefab,	true);
	}

	#endregion
}