using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class SpriteRSExtension : ObjectRSExtension
{
	#region Constants
	
	private 	const	string		kTextureKey						= "texture";
	private 	const	string		kRectKey						= "rect";
	private 	const	string		kPixelsPerUnitKey				= "pixelsPerUnit";
	private 	const	string		kPivotKey						= "pivot";
	private 	const	string		kBorderKey						= "border";
	
	#endregion
	
	#region Instance Method
	
	public override object CreateInstance (RuntimeSerializationInfo _info)
	{
		Texture2D		_texture	= _info.GetValue<Texture2D>(kTextureKey, 	true);
		Rect 			_rect		= _info.GetValue<Rect>(kRectKey,			true);
		Vector2			_pivot		= _info.GetValue<Vector2>(kPivotKey,		true);
		float			_PPU		= _info.GetValue<float>(kPixelsPerUnitKey, 	true);
		Vector4			_border		= _info.GetValue<Vector4>(kBorderKey,		true);
		
		return Sprite.Create(_texture, _rect, _pivot, _PPU, 0u, SpriteMeshType.FullRect, _border);
	}
	
	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Sprite			_sprite		= _object as Sprite;

		if (_sprite == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		Bounds			_bounds		= _sprite.bounds;
		Vector2 		_pivot 		= new Vector2((_bounds.center.x / _bounds.extents.x * -0.5f) + 0.5f, (_bounds.center.y / _bounds.extents.y * -0.5f) + 0.5f);

		_info.AddValue<Texture2D>(kTextureKey, 			_sprite.texture,		true);
		_info.AddValue<Rect>(kRectKey, 					_sprite.rect,			true);
		_info.AddValue<Vector2>(kPivotKey,				_pivot,					true);
		_info.AddValue<float>(kPixelsPerUnitKey,		_sprite.pixelsPerUnit,	true);
		_info.AddValue<Vector4>(kBorderKey,				_sprite.border,			true);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{	
		return base.ReadSerializationData(_object, _info);
	}
	
	#endregion
}