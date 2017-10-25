using UnityEngine;
using System.Collections;
using System.Collections.Generic;

namespace VoxelBusters.Utility
{
	public static class GenericsExtension 
	{
		#region IEnumerator Methods

		public static object[] ToArray (this IEnumerator _enumerator)
		{
			if (_enumerator == null)
				return null;
			
			List<object>	_objectList	= new List<object>();
			
			while (_enumerator.MoveNext())
				_objectList.Add(_enumerator.Current);
			
			return _objectList.ToArray();
		}

		#endregion

		#region List Methods

		public static object[] ToArray (this IList _listObject)
		{
			if (_listObject == null)
				return null;

			int			_count			= _listObject.Count;
			object[]	_objectArray	= new object[_count];

			for (int _iter = 0; _iter < _count; _iter++)
				_objectArray[_iter]		= _listObject[_iter];

			return _objectArray;
		}

		#endregion

		#region ICollection Methods

		public static object[] ToArray (this ICollection _collection)
		{
			if (_collection == null)
				return null;

			IEnumerator _enumerator		= _collection.GetEnumerator();
			int			_count			= _collection.Count;
			object[]	_objectArray	= new object[_count];
			int 		_iter			= 0;
			
			while (_enumerator.MoveNext())
				_objectArray[_iter++]	= _enumerator.Current;

			return _objectArray;
		}

		#endregion
	}
}