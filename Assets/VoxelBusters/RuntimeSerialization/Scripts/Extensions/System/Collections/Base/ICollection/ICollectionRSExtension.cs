using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;
using VoxelBusters.Utility;

public abstract class ICollectionRSExtension : IRuntimeSerializableExtension 
{
	#region Constants
	
	protected		const		string		kValuesKey		= "values";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		ICollection		_collection		= _object as ICollection;
		
		if (_collection == null)
			return;
		
		// Serialize properties
		_info.AddValue<object[]>(kValuesKey,	_collection.ToArray());
	}
	
	#endregion
}