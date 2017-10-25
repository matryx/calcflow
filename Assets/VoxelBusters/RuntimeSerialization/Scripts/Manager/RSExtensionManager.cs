using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;
using VoxelBusters.RuntimeSerialization.Internal;

/// <summary>
/// Manages extension selection to delegate serialization and deserialization process. You can Add, Remove and Get extensions of a specific type.
/// </summary>
public class RSExtensionManager 
{
	#region Static Properties
	
	private 	static		Dictionary<Type, RSExtension>	extensionCollection	= new Dictionary<Type, RSExtension>();
	
	#endregion
	
	#region Static Methods
	
	/// <summary>
	/// Adds the new serialization extension to its collection. If extension information is manually provided, then serialization system avoids using Reflection for finding type's extension at runtime. 
	/// </summary>
	/// <param name="_objectType">The Type for which the extension is required.</param>
	/// <param name="_extensionType">The object called while serializing and deserializing.</param>
	public static void AddNewExtension (Type _objectType, object _extension)
	{
		if (extensionCollection.ContainsKey(_objectType))
			return;
		
		extensionCollection[_objectType]	= new RSExtension(_objectType, _extension);
	}
	
	/// <summary>
	/// Removes the extension from its collections.
	/// </summary>
	/// <param name="_objectType">The Type for which the extension has to be removed.</param>
	public static void RemoveExtension (Type _objectType)
	{
		extensionCollection.Remove(_objectType);
	}
	
	/// <summary>
	/// Returns the extension for a particular type.
	/// </summary>
	/// <returns>The extension for a particular type.</returns>
	/// <param name="_objectType">The type for which extension is requested.</param>
	public static RSExtension GetExtension (Type _objectType)
	{
		RSExtension 	_extension;
		
		lock (extensionCollection)
		{
			if (!extensionCollection.TryGetValue(_objectType, out _extension))
			{
				_extension							= new RSExtension(_objectType, false);
				extensionCollection[_objectType]	= _extension;
			}
		}
		
		return _extension;
	}
	
	#endregion
}