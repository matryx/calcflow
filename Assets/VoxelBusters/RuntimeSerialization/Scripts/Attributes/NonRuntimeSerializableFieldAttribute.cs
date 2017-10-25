using UnityEngine;
using System.Collections;
using System;
using System.Reflection;

namespace VoxelBusters.RuntimeSerialization
{
	/// <summary>
	/// Indicates that a field of a <see cref="RuntimeSerializableAttribute"/> class should not be serialized at runtime.
	/// </summary>
	[AttributeUsage(AttributeTargets.Field, AllowMultiple = false)]
	public class NonRuntimeSerializedFieldAttribute : Attribute 
	{
		#region Constructors
		
		public NonRuntimeSerializedFieldAttribute ()
		{}
		
		#endregion
	}
}