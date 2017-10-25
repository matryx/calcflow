using UnityEngine;
using System.Collections;
using UnityEngine.UI;
using VoxelBusters.RuntimeSerialization;

public class ShadowRSExtension : BaseVertexEffectRSExtension 
{
	#region Constants
	
	private 	const	string		kEffectColorKey			= "effectColor";
	private 	const	string		kEffectDistanceKey		= "effectDistance";
	private 	const	string		kUseGraphicAlphaKey		= "useGraphicAlpha";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Shadow 		_shadow		= _object as Shadow;
		
		if (_shadow == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<Color>(kEffectColorKey, 			_shadow.effectColor);
		_info.AddValue<Vector2>(kEffectDistanceKey,		_shadow.effectDistance);
		_info.AddValue<bool>(kUseGraphicAlphaKey,		_shadow.useGraphicAlpha);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Shadow 		_shadow		= base.ReadSerializationData(_object, _info) as Shadow;
		
		if (_shadow == null)
			return null;
		
		// Deserialize properties
		_shadow.effectColor		= _info.GetValue<Color>(kEffectColorKey);
		_shadow.effectDistance	= _info.GetValue<Vector2>(kEffectDistanceKey);
		_shadow.useGraphicAlpha	= _info.GetValue<bool>(kUseGraphicAlphaKey);
		
		return _shadow;
	}
	
	#endregion
}
