using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public static class EnumerationExtensions 
	{
		public static int GetValue (this System.Enum _enum) 
		{
			int _finalValue			= 0;
			System.Type	_enumType	= _enum.GetType();

			foreach (int _value in System.Enum.GetValues(_enumType))
			{
				if (((int)(object)_enum & _value) != 0)
					_finalValue |= _value;
			}

			return _finalValue;
		}
	}
}
