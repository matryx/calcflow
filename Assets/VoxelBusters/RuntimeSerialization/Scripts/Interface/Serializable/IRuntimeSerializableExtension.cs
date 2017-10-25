using UnityEngine;
using System.Collections;
using System;

namespace VoxelBusters.RuntimeSerialization
{
	/// <summary>
	/// Implement this abstract class to support runtime serialization for classes which belong to external assembly.
	/// </summary>
	public abstract class IRuntimeSerializableExtension
	{
		#region Methods

		/// <summary>
		/// Creates the instance of formerly serialized object type.
		/// </summary>
		/// <returns>Serialized object instance.</returns>
		/// <param name="_info">The <see cref="RuntimeSerializationInfo"/> provides the interface to access all the previously serialized initializer values.</param>
		public virtual object CreateInstance (RuntimeSerializationInfo _info)
		{
			return Activator.CreateInstance(_info.ObjectType);
		}

		/// <summary>
		/// Populate <see cref="RuntimeSerializationInfo"/> with the properties required to serialize target object.
		/// </summary>
		/// <param name="_object">Target object to be serialized</param>
		/// <param name="_info">The <see cref="RuntimeSerializationInfo"/> provides interface to add properties to be serialized.</param>
		public abstract void WriteSerializationData (object _object, RuntimeSerializationInfo _info);
		
		/// <summary>
		/// Retrieve serialized properties of target object from <see cref="RuntimeSerializationInfo"/>.
		/// </summary>
		/// <returns>The deserialized object.</returns>
		/// <param name="_object">Target object to be deserialized.</param>
		/// <param name="_info">The <see cref="RuntimeSerializationInfo"/> provides interface to retrieve serialized values.</param>
		public abstract object ReadSerializationData (object _object, RuntimeSerializationInfo _info);
		
		#endregion
	}
}