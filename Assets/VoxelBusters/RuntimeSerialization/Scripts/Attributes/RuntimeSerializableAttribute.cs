using UnityEngine;
using System.Collections;
using System;
using System.Reflection;

/// <summary>
/// The VoxelBusters.RuntimeSerialization namespace contains classes that can be used for serializing and deserializing objects at runtime.
/// Serialization is a process of converting an object into binary stream of data and storing it on disk.
/// Deserialization is a process of reading stored data and recreating objects from it.
/// 
/// The <see cref="IRuntimeSerializable"/> interface provides a way to control their own serialization behaviour.
/// The <see cref="IRuntimeSerializableExtension"/> interface provides a way to support runtime serialization for classes which belong to external assembly.
/// </summary>
namespace VoxelBusters.RuntimeSerialization
{
	/// <summary>
	/// Indicates that a class can be serialized at runtime.
	/// </summary>
	[AttributeUsage(AttributeTargets.Assembly | AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Interface, Inherited = false, AllowMultiple = false)]
	public class RuntimeSerializableAttribute : Attribute 
	{
		#region Properties

		internal Type ExtensionDependencyObjectType
		{
			get;
			private set;
		}

		internal bool SerializeAllPublicVariables
		{
			get;
			private set;
		}

		internal bool SerializeAllNonPublicVariables
		{
			get;
			private set;
		}

		#endregion

		#region Constructors

		/// <summary>
		/// Initializes a new instance of the <see cref="RuntimeSerializableAttribute"/> class.
		/// </summary>
		/// <param name="_serializeAllPublicVariables">If set to <c>true</c> all public variables will be serialized at runtime.</param>
		/// <param name="_serializeAllNonPublicVariables">If set to <c>true</c> all non public variables will be serialized at runtime.</param>
		public RuntimeSerializableAttribute (bool _serializeAllPublicVariables, bool _serializeAllNonPublicVariables) : this (null, _serializeAllPublicVariables, _serializeAllNonPublicVariables)
		{}
	
		/// <summary>
		/// Initializes a new instance of the <see cref="RuntimeSerializableAttribute"/> class.
		/// </summary>
		/// <param name="_extensionDependencyObjectType">Its possible that class might be deriving properties from a class which belongs to an external library. 
		/// So at that point, this property can be used to create a virtual link between derived class and serialization extension of parent class to ensure that properties are properly serialized.</param>
		/// <param name="_serializeAllPublicVariables">If set to <c>true</c> all public variables will be serialized at runtime.</param>
		/// <param name="_serializeAllNonPublicVariables">If set to <c>true</c> all non public variables will be serialized at runtime.</param>
		public RuntimeSerializableAttribute (Type _extensionDependencyObjectType = null, bool _serializeAllPublicVariables = true, bool _serializeAllNonPublicVariables = false)
		{
			ExtensionDependencyObjectType	= _extensionDependencyObjectType;
			SerializeAllPublicVariables		= _serializeAllPublicVariables;
			SerializeAllNonPublicVariables	= _serializeAllNonPublicVariables;
		}

		#endregion
	}
}