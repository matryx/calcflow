using UnityEngine;
using System.Collections;
using System;
using System.Reflection;
using VoxelBusters.RuntimeSerialization;

public class QueueRSExtension: ICollectionRSExtension  
{
	#region Serialization Methods
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Type		_objectType		= _info.ObjectType;
		MethodInfo 	_clearMethod	= _objectType.GetMethod("Clear");
		MethodInfo	_enqueueMethod	= _objectType.GetMethod("Enqueue");

		if (_clearMethod == null || _enqueueMethod == null)
		{
			Debug.LogError("[RS] Failed to deserialize instance of type Queue.");
			return null;
		}
		
		// Clear
		_clearMethod.Invoke(_object, null);
		
		// Deserialize properties
		object[]	_valuesList		= _info.GetValue<object[]>(kValuesKey);
		
		if (_valuesList != null)
		{
			int		_valueCount		= _valuesList.Length;
			
			for (int _iter = 0; _iter < _valueCount; _iter++)
				_enqueueMethod.Invoke(_object, new object[] { _valuesList[_iter] });

			return _object;
		}
		
		Debug.LogError("[RS] Failed to deserialize Queue elements.");
		return _object;
	}
	
	#endregion
}