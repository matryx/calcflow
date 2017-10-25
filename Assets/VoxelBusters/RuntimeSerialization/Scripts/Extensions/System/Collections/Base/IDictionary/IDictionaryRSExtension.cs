using UnityEngine;
using System.Collections;
using System;
using VoxelBusters.RuntimeSerialization;
using VoxelBusters.Utility;

public class IDictionaryRSExtension : IRuntimeSerializableExtension 
{
	#region Constants
	
	private		const		string		kKeysKey		= "keys";
	private		const		string		kValuesKey		= "values";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		IDictionary		_dictObject		= _object as IDictionary;
		
		if (_dictObject == null)
			return;
		
		// Serialize properties
		_info.AddValue<object[]>(kKeysKey, 		_dictObject.Keys.ToArray());
		_info.AddValue<object[]>(kValuesKey, 	_dictObject.Values.ToArray());
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		IDictionary		_dictObject		= _object as IDictionary;

		if (_dictObject == null)
		{
			Debug.LogError("[RS] Failed to deserialize instance of type IDictionary.");
			return null;
		}

		// Clear all existing keys
		_dictObject.Clear();

		// Deserialize properties
		object[]		_keys			= _info.GetValue<object[]>(kKeysKey); 
		object[]		_values			= _info.GetValue<object[]>(kValuesKey); 

		if (_keys != null && _values != null)	
		{
			int 		_keyCount		= _keys.Length;
			int 		_valueCount		= _values.Length;
			
			if (_keyCount == _valueCount)
			{
				for (int _iter = 0; _iter < _keyCount; _iter++)
				{
					object	_currentKey	= _keys[_iter];

					if (_currentKey == null)
						continue;

					_dictObject.Add(_currentKey, _values[_iter]);
				}

				return _dictObject;
			}
		}

		Debug.LogError("[RS] Failed to deserialize IDictionary entries.");
		return _dictObject;
	}
	
	#endregion
}