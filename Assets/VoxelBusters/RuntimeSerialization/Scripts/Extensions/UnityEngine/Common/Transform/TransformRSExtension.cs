using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class TransformRSExtension : ComponentRSExtension 
{
	#region Constants

	private 	const 		string			kLocalPositionKey		= "localPosition";
	private 	const 		string			kLocalRotationKey		= "localRotation";
	private 	const 		string			kLocalScaleKey			= "localScale";
	private 	const 		string			kChildrenKey			= "children";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Transform 	_transform	= _object as Transform;

		if (_transform == null)
			return;
		
		UIDSystem	_UIDSystem	= _transform.GetComponent<UIDSystem>();
		
		if (_UIDSystem == null)
		{
			Debug.LogError(string.Format("[RS] UIDSystem not found in GameObject with name {0}. Please add UIDSystem component to serializing object.", _transform.name), _transform);
			return;
		}

		// Serialize base properties
		base.WriteSerializationData(_object, _info);
	
		// Serializing properties
		_info.AddValue<Vector3>(kLocalPositionKey, 		_transform.localPosition);
		_info.AddValue<Quaternion>(kLocalRotationKey, 	_transform.localRotation);
		_info.AddValue<Vector3>(kLocalScaleKey, 		_transform.localScale);

		// Serialize immediate childrens
		if (_UIDSystem.SerializeChildren)
		{
			IEnumerator _enumerator	= _transform.GetEnumerator();
			int			_iter		= 0;

			while (_enumerator.MoveNext ())
				_info.AddValue<GameObject>((kChildrenKey + _iter++), ((Transform)_enumerator.Current).gameObject);
		}
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		// Get base properties
		Transform _transform		= base.ReadSerializationData(_object, _info) as Transform;

		if (_transform == null)
			return null;

		// Get transform properties
		_transform.localPosition	= _info.GetValue<Vector3>(kLocalPositionKey);
		_transform.localRotation	= _info.GetValue<Quaternion>(kLocalRotationKey);
		_transform.localScale		= _info.GetValue<Vector3>(kLocalScaleKey);

		return _transform;
	}

	#endregion
}