using UnityEngine;
using System.Collections;
using System;
using System.Reflection;
using VoxelBusters.RuntimeSerialization;

public class StackRSExtension : ICollectionRSExtension  
{
	#region Serialization Methods
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Type		_objectType		= _info.ObjectType;
		MethodInfo 	_clearMethod	= _objectType.GetMethod("Clear");
		MethodInfo	_pushMethod		= _objectType.GetMethod("Push");

		if (_clearMethod == null || _pushMethod == null)
		{
			Debug.LogError("[RS] Failed to deserialize instance of type Stack.");
			return null;
		}

		// Clear
		_clearMethod.Invoke(_object, null);

		// Deserialize properties
		object[]	_valuesList		= _info.GetValue<object[]>(kValuesKey);

		if (_valuesList != null)
		{
			int		_valueCount		= _valuesList.Length;
			
			for (int _iter = _valueCount - 1; _iter >= 0; _iter--)
				_pushMethod.Invoke(_object, new object[] { _valuesList[_iter] });

			return _object;
		}

		Debug.LogError("[RS] Failed to deserialize Stack elements.");
		return _object;
	}
	
	#endregion
}