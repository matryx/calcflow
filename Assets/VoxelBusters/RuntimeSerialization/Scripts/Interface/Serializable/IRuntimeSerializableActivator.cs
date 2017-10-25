using UnityEngine;
using System.Collections;

namespace VoxelBusters.RuntimeSerialization
{
	/// <summary>
	/// Implement this interface to control instance creation of serialized type.
	/// Static method with signature: object CreateInstance (RuntimeSerializationInfo _info) is invoked when creating serialized object instance.
	/// <see cref="RuntimeSerializationInfo"/> contains all the initializer values that were formerly serialized. So get all the initializer values from <see cref="RuntimeSerializationInfo"/> and create object instance.
	/// </summary>
	public interface IRuntimeSerializableActivator 
	{}
}