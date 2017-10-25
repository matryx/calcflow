using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class EdgeCollider2DRSExtension : Collider2DRSExtension 
{
	#region Constants
	
	private 	const	string		kPointsKey			= "points";

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		EdgeCollider2D	_collider	= _object as EdgeCollider2D;
		
		if (_collider == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<Vector2[]>(kPointsKey, 	_collider.points);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		EdgeCollider2D	_collider	= base.ReadSerializationData(_object, _info) as EdgeCollider2D;
		
		if (_collider == null)
			return null;
		
		// Deserialize properties
		_collider.points			= _info.GetValue<Vector2[]>(kPointsKey);

		return _collider;
	}
	
	#endregion
}