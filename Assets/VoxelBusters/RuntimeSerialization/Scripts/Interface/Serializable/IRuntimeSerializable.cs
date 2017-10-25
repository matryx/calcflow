using UnityEngine;
using System.Collections;

namespace VoxelBusters.RuntimeSerialization
{
	/// <summary>
	/// Implement this interface to control serialization and deserialization of <see cref="RuntimeSerializableAttribute"/> object.
	/// </summary>
	public interface IRuntimeSerializable
	{
		#region Methods

		/// <summary>
		/// Populate <see cref="RuntimeSerializationInfo"/> with the properties required to serialize <see cref="RuntimeSerializableAttribute"/> object.
		/// </summary>
		/// <param name="_info">The <see cref="RuntimeSerializationInfo"/> provides interface to store properties of <see cref="RuntimeSerializableAttribute"/> object to be serialized.</param>
		void WriteSerializationData (RuntimeSerializationInfo _info);

		/// <summary>
		/// Retrieve properties of <see cref="RuntimeSerializableAttribute"/> object from <see cref="RuntimeSerializationInfo"/>.
		/// </summary>
		/// <returns>Object retrieved from deserializing serialization data.</returns>
		/// <param name="_info">The <see cref="RuntimeSerializationInfo"/> provides interface to retrieve <see cref="RuntimeSerializableAttribute"/> object property values.</param>
		object ReadSerializationData (RuntimeSerializationInfo _info);

		#endregion
	}
}