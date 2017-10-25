using UnityEngine;
using System.Collections;
using System;
using System.Reflection;
using VoxelBusters.RuntimeSerialization;
using VoxelBusters.Utility;

public class HashSetRSExtension : IRuntimeSerializableExtension 
{
	#region Constants
	
	protected		const		string		kValuesKey		= "values";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		IEnumerator		_enumerator		= (_object as IEnumerable).GetEnumerator();
		
		if (_enumerator == null)
			return;
		
		// Serialize properties
		_info.AddValue<object[]>(kValuesKey,	_enumerator.ToArray());
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Type		_objectType		= _info.ObjectType;
		MethodInfo 	_clearMethod	= _objectType.GetMethod("Clear");
		MethodInfo	_addMethod		= _objectType.GetMethod("Add");
		
		if (_clearMethod == null || _addMethod == null)
		{
			Debug.LogError("[RS] Failed to deserialize instance of type HashSet.");
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
				_addMethod.Invoke(_object, new object[] { _valuesList[_iter] });
			
			return _object;
		}
		
		Debug.LogError("[RS] Failed to deserialize HashSet elements.");
		return _object;
	}
	
	#endregion
}