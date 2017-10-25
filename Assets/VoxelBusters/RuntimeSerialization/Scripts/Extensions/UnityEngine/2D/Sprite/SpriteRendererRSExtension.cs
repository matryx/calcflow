using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class SpriteRendererRSExtension : RendererRSExtension 
{
	#region Constants
	
	private 	const	string		kColorKey		= "color";
	private 	const	string		kSpriteKey		= "sprite";

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SpriteRenderer	_renderer	= _object as SpriteRenderer;

		if (_renderer == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<Color>(kColorKey, 	_renderer.color);
		_info.AddValue<Sprite>(kSpriteKey,	_renderer.sprite);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		// Deserialize base properties
		SpriteRenderer	_renderer	= base.ReadSerializationData(_object, _info) as SpriteRenderer;

		if (_renderer == null)
			return null;

		// Deserialize properties
		_renderer.color				= _info.GetValue<Color>(kColorKey);
		_renderer.sprite			= _info.GetValue<Sprite>(kSpriteKey);

		return _renderer;
	}
	
	#endregion
}