using UnityEngine;
using System.Collections;
using System;
using VoxelBusters.RuntimeSerialization;

public class TimeSpanRSExtension : IRuntimeSerializableExtension
{
	#region Constants
	
	private		const		string			kTicksKey	= "ticks";
	
	#endregion
	
	#region Instance Method
	
	public override object CreateInstance (RuntimeSerializationInfo _info)
	{
		return new TimeSpan(_info.GetValue<long>(kTicksKey, true));
	}
	
	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		TimeSpan	_timeSpan		= (TimeSpan)_object;

		// Write properties
		_info.AddValue<long>(kTicksKey, _timeSpan.Ticks, true);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		return _object;
	}
	
	#endregion
}
