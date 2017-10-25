using UnityEngine;
using System.Collections;
using System;
using System.Reflection;
using VoxelBusters.Utility;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	public struct Field
	{
		#region Properties
		
		public FieldInfo Info
		{
			get;
			private set;
		}
		
		public bool IsObjectInitializer
		{
			get;
			private set;
		}
		
		#endregion
		
		#region Constructors
		
		public Field (FieldInfo _fieldInfo) : this()
		{
			if (_fieldInfo == null)
				throw new ArgumentNullException("[RS] Field info is null.");

			// Get attribute associated with this field
			RuntimeSerializeFieldAttribute 	_serializeAttribute	= _fieldInfo.GetAttribute<RuntimeSerializeFieldAttribute>(false);

			// Initialize
			Info				= _fieldInfo;
			IsObjectInitializer	= (_serializeAttribute == null) ? false : _serializeAttribute.IsObjectInitializer;
		}

		public Field (FieldInfo _fieldInfo, RuntimeSerializeFieldAttribute _serializeAttribute) : this()
		{
			if (_fieldInfo == null)
				throw new ArgumentNullException("[RS] Field info is null.");

			if (_serializeAttribute == null)
				throw new ArgumentNullException("[RS] Attribute is null.");

			// Initialize
			Info				= _fieldInfo;
			IsObjectInitializer	= _serializeAttribute.IsObjectInitializer;
		}
		
		#endregion
	}
}