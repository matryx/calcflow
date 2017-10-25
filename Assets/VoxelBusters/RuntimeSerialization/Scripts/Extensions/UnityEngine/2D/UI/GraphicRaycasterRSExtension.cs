using UnityEngine;
using System.Collections;
using UnityEngine.UI;
using VoxelBusters.RuntimeSerialization;

public class GraphicRaycasterRSExtension : BaseRaycasterRSExtension 
{
	#region Constants
	
	private 	const	string		kBlockingObjectsKey			= "blockingObjects";
	private 	const	string		kIgnoreReversedGraphicsKey	= "ignoreReversedGraphics";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		GraphicRaycaster	_graphicRaycaster	= _object as GraphicRaycaster;
		
		if (_graphicRaycaster == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<GraphicRaycaster.BlockingObjects>(kBlockingObjectsKey, 	_graphicRaycaster.blockingObjects);
		_info.AddValue<bool>(kIgnoreReversedGraphicsKey, 						_graphicRaycaster.ignoreReversedGraphics);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		GraphicRaycaster 	_graphicRaycaster		= base.ReadSerializationData(_object, _info) as GraphicRaycaster;
		
		if (_graphicRaycaster == null)
			return null;
		
		// Deserialize properties
		_graphicRaycaster.blockingObjects			= _info.GetValue<GraphicRaycaster.BlockingObjects>(kBlockingObjectsKey);
		_graphicRaycaster.ignoreReversedGraphics	= _info.GetValue<bool>(kIgnoreReversedGraphicsKey);
		
		return _graphicRaycaster;
	}
	
	#endregion
}
