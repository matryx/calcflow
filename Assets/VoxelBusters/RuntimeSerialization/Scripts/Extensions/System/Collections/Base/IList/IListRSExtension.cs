using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;
using VoxelBusters.Utility;

public class IListRSExtension : ICollectionRSExtension 
{
	#region Serialization Methods
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		IList		_listObject		= _object as IList;

		if (_listObject == null)
		{
			Debug.LogError("[RS] Failed to deserialize instance of type IList.");
			return null;
		}

		// Remove existing values
		_listObject.Clear();

		// Deserialize properties
		object[]	_valuesList		= _info.GetValue<object[]>(kValuesKey);

		if (_valuesList != null)
		{
			int		_valueCount		= _valuesList.Length;
			
			for (int _iter = 0; _iter < _valueCount; _iter++)
				_listObject.Add(_valuesList[_iter]);

			return _listObject;
		}

		Debug.LogError("[RS] Failed to deserialize IList elements.");
		return _listObject;
	}
	
	#endregion
}