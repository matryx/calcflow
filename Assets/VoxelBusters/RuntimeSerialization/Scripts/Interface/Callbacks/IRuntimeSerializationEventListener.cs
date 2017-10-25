using UnityEngine;
using System.Collections;

namespace VoxelBusters.RuntimeSerialization
{
	/// <summary>
	/// Implement this interface to observe serialization process of <see cref="RuntimeSerializableAttribute"/> object.
	/// </summary>
	public interface IRuntimeSerializationEventListener
	{
		/// <summary>
		/// Event triggered after <see cref="RuntimeSerializableAttribute"/> object serialization is completed.
		/// </summary>
		/// <param name="_key">A key string used to identify object's serialization.</param>
		/// <param name="_object">The object which was just serialized.</param>
		void OnAfterRuntimeSerialize (string _key, object _object);

		/// <summary>
		/// Event triggered after <see cref="RuntimeSerializableAttribute"/> object deserialization is completed.
		/// </summary>
		/// <param name="_key">A key string used to identify object's deserialization.</param>
		/// <param name="_object">The object retrieved from deserializing serialization data.</param>
		void OnAfterRuntimeDeserialize (string _key, object _object);
	}
}