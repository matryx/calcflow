using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class RectRSExtension : IRuntimeSerializableExtension
{
	#region Constants
	
	private 	const	string		kOriginXKey			= "x";
	private 	const	string		kOriginYKey			= "y";
	private 	const	string		kWidthKey			= "w";
	private 	const	string		kHeightKey			= "h";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Rect	_rect		= (Rect)_object;

		// Serialize properties
		_info.AddValue<float>(kOriginXKey, 	_rect.x);
		_info.AddValue<float>(kOriginYKey,	_rect.y);
		_info.AddValue<float>(kWidthKey, 	_rect.width);
		_info.AddValue<float>(kHeightKey, 	_rect.height);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Rect	_rect		= (Rect)_object;

		// Deserialize properties
		_rect.x				= _info.GetValue<float>(kOriginXKey);
		_rect.y				= _info.GetValue<float>(kOriginYKey);
		_rect.width 		= _info.GetValue<float>(kWidthKey);
		_rect.height		= _info.GetValue<float>(kHeightKey);

		return _rect;
	}
	
	#endregion
}