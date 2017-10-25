using UnityEngine;
using System.Collections;
using UnityEngine.UI;
using VoxelBusters.RuntimeSerialization;

public class CanvasScalerRSExtension : BehaviourRSExtension 
{
	#region Constants
	
	private		const 		string			kDefaultSpriteDPIKey			= "defaultSpriteDPI";
	private		const 		string			kDynamicPixelsPerUnitKey		= "dynamicPixelsPerUnit";
	private		const 		string			kFallbackScreenDPIKey			= "fallbackScreenDPI";
	private		const 		string			kMatchWidthOrHeightKey			= "matchWidthOrHeight";
	private		const 		string			kPhysicalUnitKey				= "physicalUnit";
	private		const 		string			kReferencePixelsPerUnitKey		= "referencePixelsPerUnit";
	private		const 		string			kReferenceResolutionKey			= "referenceResolution";
	private		const 		string			kScaleFactorKey					= "scaleFactor";
	private		const 		string			kScreenMatchModeKey				= "screenMatchMode";
	private		const 		string			kUIScaleModeKey					= "uiScaleMode";

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		CanvasScaler 		_canvasScaler	= _object as CanvasScaler;
		
		if (_canvasScaler == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<float>(kDefaultSpriteDPIKey, 						_canvasScaler.defaultSpriteDPI);
		_info.AddValue<float>(kDynamicPixelsPerUnitKey, 					_canvasScaler.dynamicPixelsPerUnit);
		_info.AddValue<float>(kFallbackScreenDPIKey, 						_canvasScaler.fallbackScreenDPI);
		_info.AddValue<float>(kMatchWidthOrHeightKey,	 					_canvasScaler.matchWidthOrHeight);
		_info.AddValue<CanvasScaler.Unit>(kPhysicalUnitKey,				 	_canvasScaler.physicalUnit);
		_info.AddValue<float>(kReferencePixelsPerUnitKey, 					_canvasScaler.referencePixelsPerUnit);
		_info.AddValue<Vector2>(kReferenceResolutionKey, 					_canvasScaler.referenceResolution);
		_info.AddValue<float>(kScaleFactorKey, 								_canvasScaler.scaleFactor);
		_info.AddValue<CanvasScaler.ScreenMatchMode>(kScreenMatchModeKey, 	_canvasScaler.screenMatchMode);
		_info.AddValue<CanvasScaler.ScaleMode>(kUIScaleModeKey, 			_canvasScaler.uiScaleMode);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		CanvasScaler 		_canvasScaler		= base.ReadSerializationData(_object, _info) as CanvasScaler;
		
		if (_canvasScaler == null)
			return null;
		
		// Deserialize properties	
		_canvasScaler.defaultSpriteDPI			= _info.GetValue<float>(kDefaultSpriteDPIKey);
		_canvasScaler.dynamicPixelsPerUnit		= _info.GetValue<float>(kDynamicPixelsPerUnitKey);
		_canvasScaler.fallbackScreenDPI			= _info.GetValue<float>(kFallbackScreenDPIKey);
		_canvasScaler.matchWidthOrHeight		= _info.GetValue<float>(kMatchWidthOrHeightKey);
		_canvasScaler.physicalUnit				= _info.GetValue<CanvasScaler.Unit>(kPhysicalUnitKey);
		_canvasScaler.referencePixelsPerUnit	= _info.GetValue<float>(kReferencePixelsPerUnitKey);
		_canvasScaler.referenceResolution		= _info.GetValue<Vector2>(kReferenceResolutionKey);
		_canvasScaler.scaleFactor				= _info.GetValue<float>(kScaleFactorKey);
		_canvasScaler.screenMatchMode			= _info.GetValue<CanvasScaler.ScreenMatchMode>(kScreenMatchModeKey);
		_canvasScaler.uiScaleMode				= _info.GetValue<CanvasScaler.ScaleMode>(kUIScaleModeKey);
		
		return _canvasScaler;
	}
	
	#endregion
}
