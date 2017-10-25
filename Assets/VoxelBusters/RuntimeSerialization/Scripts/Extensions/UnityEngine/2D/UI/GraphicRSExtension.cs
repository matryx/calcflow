using UnityEngine;
using System.Collections;
using UnityEngine.UI;
using VoxelBusters.RuntimeSerialization;

public class GraphicRSExtension : UIBehaviourRSExtension 
{
	#region Constants
	
	private 	const	string		kColorKey		= "color";
	private		const	string		kMaterialKey	= "material";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Graphic 	_graphic	= _object as Graphic;
		
		if (_graphic == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<Color>(kColorKey, 		_graphic.color);
		_info.AddValue<Material>(kMaterialKey, 	_graphic.material);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Graphic 	_graphic	= base.ReadSerializationData(_object, _info) as Graphic;
		
		if (_graphic == null)
			return null;
		
		// Deserialize properties
		_graphic.color			= _info.GetValue<Color>(kColorKey);
		_graphic.material		= _info.GetValue<Material>(kMaterialKey);
		
		return _graphic;
	}
	
	#endregion
}