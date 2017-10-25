using UnityEngine;
using System.Collections;
using UnityEngine.UI;
using VoxelBusters.RuntimeSerialization;

public class MaskableGraphicRSExtension : GraphicRSExtension 
{
	#region Constants
	
	private 	const	string		kMaskableKey	= "maskable";

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		MaskableGraphic		_graphic	= _object as MaskableGraphic;
		
		if (_graphic == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<bool>(kMaskableKey, 	_graphic.maskable);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		MaskableGraphic 	_graphic	= base.ReadSerializationData(_object, _info) as MaskableGraphic;
		
		if (_graphic == null)
			return null;
		
		// Deserialize properties
		_graphic.maskable				= _info.GetValue<bool>(kMaskableKey);
		
		return _graphic;
	}
	
	#endregion
}