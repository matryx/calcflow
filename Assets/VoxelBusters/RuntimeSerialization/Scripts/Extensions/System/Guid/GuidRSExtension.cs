using UnityEngine;
using System.Collections;
using System;
using VoxelBusters.RuntimeSerialization;

public class GuidRSExtension : IRuntimeSerializableExtension
{
	#region Constants

	private		const		string			kDataKey	= "data";

	#endregion
	
	#region Instance Method
	
	public override object CreateInstance (RuntimeSerializationInfo _info)
	{
		return new Guid(_info.GetValue<byte[]>(kDataKey, true));
	}
	
	#endregion

	#region Serialization Methods

	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Guid	_guid		= (Guid)_object;

		// Serialize properties
		_info.AddValue<byte[]>(kDataKey, _guid.ToByteArray(), true);
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		return _object;
	}

	#endregion
}
