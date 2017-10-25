using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class CanvasRSExtension : BehaviourRSExtension 
{
	#region Constants
	
	private 	const	string		kOverridePixelPerfectKey		= "overridePixelPerfect";
	private 	const	string		kOverrideSortingKey				= "overrideSorting";
	private 	const	string		kPixelPerfectKey				= "pixelPerfect";
	private 	const	string		kPlaneDistanceKey				= "planeDistance";
	private 	const	string		kReferencePixelsPerUnitKey		= "referencePixelsPerUnit";
	private 	const	string		kRenderModeKey					= "renderMode";
	private 	const	string		kScaleFactorKey					= "scaleFactor";
	private 	const	string		kSortingLayerIDKey				= "sortingLayerID";
	private 	const	string		kSortingLayerNameKey			= "sortingLayerName";
	private 	const	string		kSortingOrderKey				= "sortingOrder";

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Canvas 		_canvas				= _object as Canvas;
		
		if (_canvas == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<bool>(kOverridePixelPerfectKey, 		_canvas.overridePixelPerfect);
		_info.AddValue<bool>(kOverrideSortingKey, 			_canvas.overrideSorting);
		_info.AddValue<bool>(kPixelPerfectKey, 				_canvas.pixelPerfect);
		_info.AddValue<float>(kPlaneDistanceKey,		 	_canvas.planeDistance);
		_info.AddValue<float>(kReferencePixelsPerUnitKey, 	_canvas.referencePixelsPerUnit);
		_info.AddValue<RenderMode>(kRenderModeKey, 			_canvas.renderMode);
		_info.AddValue<float>(kScaleFactorKey, 				_canvas.scaleFactor);
		_info.AddValue<int>(kSortingLayerIDKey, 			_canvas.sortingLayerID);
		_info.AddValue<string>(kSortingLayerNameKey, 		_canvas.sortingLayerName);
		_info.AddValue<int>(kSortingOrderKey,				_canvas.sortingOrder);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Canvas	 	_canvas				= base.ReadSerializationData(_object, _info) as Canvas;
		
		if (_canvas == null)
			return null;
		
		// Deserialize properties
		_canvas.overridePixelPerfect	= _info.GetValue<bool>(kOverridePixelPerfectKey);
		_canvas.overrideSorting			= _info.GetValue<bool>(kOverrideSortingKey);
		_canvas.pixelPerfect			= _info.GetValue<bool>(kPixelPerfectKey);
		_canvas.planeDistance			= _info.GetValue<float>(kPlaneDistanceKey);
		_canvas.referencePixelsPerUnit	= _info.GetValue<float>(kReferencePixelsPerUnitKey);
		_canvas.renderMode				= _info.GetValue<RenderMode>(kRenderModeKey);
		_canvas.scaleFactor				= _info.GetValue<float>(kScaleFactorKey);
		_canvas.sortingLayerID			= _info.GetValue<int>(kSortingLayerIDKey);
		_canvas.sortingLayerName		= _info.GetValue<string>(kSortingLayerNameKey);
		_canvas.sortingOrder			= _info.GetValue<int>(kSortingOrderKey);
		
		return _canvas;
	}
	
	#endregion
}
