using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class RectTransformRSExtension : TransformRSExtension 
{
	#region Constants

	private		const 		string			kAnchoredPosition3DKey			= "anchoredPosition3D";
	private		const 		string			kAnchorMaxKey					= "anchorMax";
	private		const 		string			kAnchorMinKey					= "anchorMin";
	private		const 		string			kOffsetMaxKey					= "offsetMax";
	private		const 		string			kOffsetMinKey					= "offsetMin";
	private		const 		string			kPivotKey						= "pivot";
	private		const 		string			kSizeDeltaKey					= "sizeDelta";
	
	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		RectTransform 		_rectTransform	= _object as RectTransform;

		if (_rectTransform == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<Vector3>(kAnchoredPosition3DKey, _rectTransform.anchoredPosition3D);
		_info.AddValue<Vector2>(kAnchorMaxKey, 			_rectTransform.anchorMax);
		_info.AddValue<Vector2>(kAnchorMinKey, 			_rectTransform.anchorMin);
		_info.AddValue<Vector2>(kOffsetMaxKey, 			_rectTransform.offsetMax);
		_info.AddValue<Vector2>(kOffsetMinKey, 			_rectTransform.offsetMin);
		_info.AddValue<Vector2>(kPivotKey, 				_rectTransform.pivot);
		_info.AddValue<Vector2>(kSizeDeltaKey, 			_rectTransform.sizeDelta);
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		RectTransform 		_rectTransform	= base.ReadSerializationData(_object, _info) as RectTransform;

		if (_rectTransform == null)
			return null;

		// Deserialize properties	
		_rectTransform.anchoredPosition3D	= _info.GetValue<Vector3>(kAnchoredPosition3DKey);
		_rectTransform.anchorMax			= _info.GetValue<Vector2>(kAnchorMaxKey);
		_rectTransform.anchorMin			= _info.GetValue<Vector2>(kAnchorMinKey);
		_rectTransform.offsetMax			= _info.GetValue<Vector2>(kOffsetMaxKey);
		_rectTransform.offsetMin			= _info.GetValue<Vector2>(kOffsetMinKey);
		_rectTransform.pivot				= _info.GetValue<Vector2>(kPivotKey);
		_rectTransform.sizeDelta			= _info.GetValue<Vector2>(kSizeDeltaKey);

		return _rectTransform;
	}

	#endregion
}