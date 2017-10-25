using UnityEngine;
using System.Collections;

namespace VoxelBusters.RuntimeSerialization
{
	/// <summary>
	/// Implement this interface to receive implicit callbacks on <see cref="RuntimeSerializableAttribute"/> object.
	/// </summary>
	public interface IRuntimeSerializationCallback 
	{
		/// <summary>
		/// Event triggered after <see cref="RuntimeSerializableAttribute"/> object is serialized.
		/// </summary>
		void OnAfterRuntimeSerialize ();

		/// <summary>
		/// Event triggered after <see cref="RuntimeSerializableAttribute"/> object is deserialized.
		/// </summary>
		void OnAfterRuntimeDeserialize ();
	}
}