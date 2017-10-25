using UnityEngine;
using System.Collections;
using System;
using System.Reflection;

namespace VoxelBusters.RuntimeSerialization
{
	/// <summary>
	/// Indicates that a field of a <see cref="RuntimeSerializableAttribute"/> class should be serialized at runtime.
	/// </summary>
	[AttributeUsage(AttributeTargets.Field, AllowMultiple = false)]
	public class RuntimeSerializeFieldAttribute : Attribute 
	{
		#region Properties

		internal bool IsObjectInitializer
		{
			get;
			private set;
		}

		#endregion

		#region Constructors

		public RuntimeSerializeFieldAttribute () : this(false)
		{}
	
		public RuntimeSerializeFieldAttribute (bool _isObjectInitializer)
		{
			IsObjectInitializer	= _isObjectInitializer;
		}

		#endregion
	}
}