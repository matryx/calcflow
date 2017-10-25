using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;
using VoxelBusters.RuntimeSerialization.Internal;

public class BehaviourRSExtension : ComponentRSExtension 
{
	#region Constants

	private 	const	string		kEnabledKey		= "enabled";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Behaviour 	_behaviour	= _object as Behaviour;
		
		if (_behaviour == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serialize properties
		_info.AddValue<bool>(kEnabledKey, _behaviour.enabled);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Behaviour _behaviour	= base.ReadSerializationData(_object, _info) as Behaviour;

		if (_behaviour == null)
			return null;

		// Deserialize properties
		_behaviour.enabled		= _info.GetValue<bool>(kEnabledKey);

		return _behaviour;
	}
	
	#endregion
}