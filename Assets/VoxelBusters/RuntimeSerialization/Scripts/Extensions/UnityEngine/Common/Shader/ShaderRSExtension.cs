using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class ShaderRSExtension : ObjectRSExtension
{
	#region Constants

	private 	const 	string		kName		= "name";

	#endregion
	
	#region Instance Method
	
	public override object CreateInstance (RuntimeSerializationInfo _info)
	{
		string	_shaderName	= _info.GetValue<string>(kName, true);
			
		return Shader.Find(_shaderName);
	}
	
	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Shader 	_shader		= _object as Shader;
		
		if (_shader == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serialize properties
		_info.AddValue<string>(kName, _shader.name, true);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		return base.ReadSerializationData(_object, _info);
	}

	#endregion
}