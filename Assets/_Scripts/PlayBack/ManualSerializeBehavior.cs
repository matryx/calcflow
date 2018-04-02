using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Nanome.Core;
using VoxelBusters.RuntimeSerialization;

public abstract class ManualSerializeBehavior : Nanome.Core.Behaviour, IRuntimeSerializationCallback
{

    /// <summary>
    /// Event triggered before <see cref="RuntimeSerializableAttribute"/> object is serialized.
    /// </summary>
    public void OnBeforeRuntimeSerialize(){
		manualSerialize();
	}

    /// <summary>
    /// Event triggered after <see cref="RuntimeSerializableAttribute"/> object is serialized.
    /// </summary>
    public virtual void OnAfterRuntimeSerialize() { }

    /// <summary>
    /// Event triggered after <see cref="RuntimeSerializableAttribute"/> object is deserialized.
    /// </summary>
    public void OnAfterRuntimeDeserialize() {
		manualDeserialize();
	}

    /// <summary>
    /// Override this to manually serialize the class. 
	/// Create and populate serializable variables that you will use to deserialize.
    /// </summary>
	protected abstract void manualSerialize();

    /// <summary>
    /// Override this to manually deserialize the class. 
	/// Use the serialized variables to reconstruct the class.
    /// </summary>
	protected abstract void manualDeserialize();


}
