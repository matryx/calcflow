using UnityEngine;
using System.Collections;
using UnityEngine.UI;
using VoxelBusters.RuntimeSerialization;

public class ImageRSExtension : MaskableGraphicRSExtension
{
	#region Constants

	private 	const	string		kEventAlphaThresholdKey		= "eventAlphaThreshold";
	private 	const	string		kFillAmountKey				= "fillAmount";
	private 	const	string		kFillCenterKey				= "fillCenter";
	private 	const	string		kFillClockwiseKey			= "fillClockwise";
	private 	const	string		kFillMethodKey				= "fillMethod";
	private 	const	string		kFillOriginKey				= "fillOrigin";
	private 	const	string		kOverrideSpriteKey			= "overrideSprite";
	private 	const	string		kPreserveAspectKey			= "preserveAspect";
	private 	const	string		kSpriteKey					= "sprite";
	private 	const	string		kTypeKey					= "type";
	
	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Image 	_image 	= _object as Image;

		if (_image == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties		
		_info.AddValue<float>(kEventAlphaThresholdKey, 		_image.alphaHitTestMinimumThreshold);
		_info.AddValue<float>(kFillAmountKey, 				_image.fillAmount);
		_info.AddValue<bool>(kFillCenterKey, 				_image.fillCenter);
		_info.AddValue<bool>(kFillClockwiseKey, 			_image.fillClockwise);		
		_info.AddValue<Image.FillMethod>(kFillMethodKey, 	_image.fillMethod);
		_info.AddValue<int>(kFillOriginKey,					_image.fillOrigin);
		_info.AddValue<Sprite>(kOverrideSpriteKey, 			_image.overrideSprite);
		_info.AddValue<bool>(kPreserveAspectKey, 			_image.preserveAspect);
		_info.AddValue<Sprite>(kSpriteKey, 					_image.sprite);
		_info.AddValue<Image.Type>(kTypeKey, 				_image.type);
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Image 	_image 				= base.ReadSerializationData(_object, _info) as Image;

		if (_image == null)
			return null;
		
		// Deserialize properties
		_image.alphaHitTestMinimumThreshold	= _info.GetValue<float>(kEventAlphaThresholdKey);
		_image.fillAmount			= _info.GetValue<float>(kFillAmountKey);
		_image.fillCenter			= _info.GetValue<bool>(kFillCenterKey);
		_image.fillClockwise		= _info.GetValue<bool>(kFillClockwiseKey);		
		_image.fillMethod			= _info.GetValue<Image.FillMethod>(kFillMethodKey);
		_image.fillOrigin			= _info.GetValue<int>(kFillOriginKey);
		_image.overrideSprite		= _info.GetValue<Sprite>(kOverrideSpriteKey);
		_image.preserveAspect		= _info.GetValue<bool>(kPreserveAspectKey);
		_image.sprite				= _info.GetValue<Sprite>(kSpriteKey);
		_image.type					= _info.GetValue<Image.Type>(kTypeKey);
		
		return _image;
	}
	
	#endregion
}