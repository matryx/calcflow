using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class CameraRSExtension : BehaviourRSExtension 
{
	#region Constants

	private 	const	string		kAspectKey							= "aspect";
	private 	const	string		kBackgroundColorKey					= "backgroundColor";
	private 	const	string		kClearFlagsKey						= "clearFlags";
	private 	const	string		kClearStencilAfterLightingPassKey	= "clearStencil";
	private 	const	string		kCullingMaskKey						= "cullingMask";
	private 	const	string		kDepthKey							= "depth";
	private 	const	string		kDepthTextureModeKey				= "depthTextureMode";
	private 	const	string		kEventMaskKey						= "eventMask";
	private 	const	string		kFarClipPlaneKey					= "farClipPlane";
	private 	const	string		kFieldOfViewKey						= "fieldOfView";
	private 	const	string		kHdrKey								= "hdr";
	private 	const	string		kLayerCullDistancesKey				= "layerCullDistances";
	private 	const	string		kLayerCullSphericalKey				= "layerCullSpherical";
	private 	const	string		kNearClipPlaneKey					= "nearClipPlane";
	private 	const	string		kOrthographicKey					= "orthographic";
	private 	const	string		kOrthographicSizeKey				= "orthographicSize";
	private 	const	string		kProjectionMatrixKey				= "projectionMatrix";
	private 	const	string		kRectKey							= "rect";
	private 	const	string		kRenderingPathKey					= "renderingPath";
	private 	const	string		kStereoConvergenceKey				= "stereoConvergence";
	private 	const	string		kStereoSeparationKey				= "stereoSeparation";
	private 	const	string		kTransparencySortModeKey			= "transparencySortMode";
	private 	const	string		kUseOcclusionCullingKey				= "useOcclusionCulling";
	private 	const	string		kWorldToCameraMatrixKey				= "worldToCameraMatrix";

#if UNITY_5
	private 	const	string		kCommandBufferCountKey				= "commandBufferCount";
	private 	const	string		kOpaqueSortModeKey					= "opaqueSortMode";
	private 	const	string		kStereoMirrorModeKey				= "stereoMirrorMode";
	private 	const	string		kTargetDisplayKey					= "targetDisplay";
#endif

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Camera 		_camera		= _object as Camera;
		
		if (_camera == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serialize properties
		_info.AddValue<float>(kAspectKey, 								_camera.aspect);
		_info.AddValue<Color>(kBackgroundColorKey, 						_camera.backgroundColor);
		_info.AddValue<CameraClearFlags>(kClearFlagsKey, 				_camera.clearFlags);
		_info.AddValue<bool>(kClearStencilAfterLightingPassKey, 		_camera.clearStencilAfterLightingPass);
		_info.AddValue<int>(kCullingMaskKey, 							_camera.cullingMask);
		_info.AddValue<float>(kDepthKey, 								_camera.depth);
		_info.AddValue<DepthTextureMode>(kDepthTextureModeKey, 			_camera.depthTextureMode);
		_info.AddValue<int>(kEventMaskKey, 								_camera.eventMask);
		_info.AddValue<float>(kFarClipPlaneKey, 						_camera.farClipPlane);
		_info.AddValue<float>(kFieldOfViewKey, 							_camera.fieldOfView);
		_info.AddValue<bool>(kHdrKey, 									_camera.hdr);
		_info.AddValue<float[]>(kLayerCullDistancesKey, 				_camera.layerCullDistances);
		_info.AddValue<bool>(kLayerCullSphericalKey, 					_camera.layerCullSpherical);
		_info.AddValue<float>(kNearClipPlaneKey, 						_camera.nearClipPlane);
		_info.AddValue<bool>(kOrthographicKey, 							_camera.orthographic);
		_info.AddValue<float>(kOrthographicSizeKey, 					_camera.orthographicSize);
		_info.AddValue<Matrix4x4>(kProjectionMatrixKey, 				_camera.projectionMatrix);
		_info.AddValue<RenderingPath>(kRenderingPathKey, 				_camera.renderingPath);
		_info.AddValue<float>(kStereoConvergenceKey, 					_camera.stereoConvergence);
		_info.AddValue<float>(kStereoSeparationKey, 					_camera.stereoSeparation);
		_info.AddValue<TransparencySortMode>(kTransparencySortModeKey, 	_camera.transparencySortMode);
		_info.AddValue<bool>(kUseOcclusionCullingKey, 					_camera.useOcclusionCulling);
		_info.AddValue<Matrix4x4>(kWorldToCameraMatrixKey, 				_camera.worldToCameraMatrix);
		

#if UNITY_5 && !UNITY_5_0
		_info.AddValue<bool>(kStereoMirrorModeKey, 						_camera.stereoMirrorMode);
		_info.AddValue<int>(kTargetDisplayKey, 							_camera.targetDisplay);

#if !(UNITY_5_1_0 || UNITY_5_1_1)
		_info.AddValue<UnityEngine.Rendering.OpaqueSortMode>(kOpaqueSortModeKey, 	_camera.opaqueSortMode);
#endif
#endif						
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		// Deserialize base properties
		Camera 		_camera						= base.ReadSerializationData(_object, _info) as Camera;
		
		if (_camera == null)
			return null;

		// Deserialize properties
		_camera.aspect							= _info.GetValue<float>(kAspectKey);
		_camera.backgroundColor					= _info.GetValue<Color>(kBackgroundColorKey);
		_camera.clearFlags						= _info.GetValue<CameraClearFlags>(kClearFlagsKey);
		_camera.clearStencilAfterLightingPass	= _info.GetValue<bool>(kClearStencilAfterLightingPassKey);
		_camera.cullingMask						= _info.GetValue<int>(kCullingMaskKey);
		_camera.depth							= _info.GetValue<float>(kDepthKey);
		_camera.depthTextureMode				= _info.GetValue<DepthTextureMode>(kDepthTextureModeKey);
		_camera.eventMask						= _info.GetValue<int>(kEventMaskKey);
		_camera.farClipPlane					= _info.GetValue<float>(kFarClipPlaneKey);
		_camera.fieldOfView						= _info.GetValue<float>(kFieldOfViewKey);
		_camera.hdr								= _info.GetValue<bool>(kHdrKey);
		_camera.layerCullDistances				= _info.GetValue<float[]>(kLayerCullDistancesKey);
		_camera.layerCullSpherical				= _info.GetValue<bool>(kLayerCullSphericalKey);
		_camera.nearClipPlane					= _info.GetValue<float>(kNearClipPlaneKey);
		_camera.orthographic					= _info.GetValue<bool>(kOrthographicKey);
		_camera.orthographicSize				= _info.GetValue<float>(kOrthographicSizeKey);
		_camera.projectionMatrix				= _info.GetValue<Matrix4x4>(kProjectionMatrixKey);
		_camera.renderingPath					= _info.GetValue<RenderingPath>(kRenderingPathKey);
		_camera.stereoConvergence				= _info.GetValue<float>(kStereoConvergenceKey);
		_camera.stereoSeparation				= _info.GetValue<float>(kStereoSeparationKey);
		_camera.transparencySortMode			= _info.GetValue<TransparencySortMode>(kTransparencySortModeKey);
		_camera.useOcclusionCulling				= _info.GetValue<bool>(kUseOcclusionCullingKey);
		_camera.worldToCameraMatrix				= _info.GetValue<Matrix4x4>(kWorldToCameraMatrixKey);
		
#if UNITY_5 && !UNITY_5_0
		_camera.stereoMirrorMode				= _info.GetValue<bool>(kStereoMirrorModeKey);
		_camera.targetDisplay					= _info.GetValue<int>(kTargetDisplayKey);

#if !(UNITY_5_1_0 || UNITY_5_1_1)
		_camera.opaqueSortMode					= _info.GetValue<UnityEngine.Rendering.OpaqueSortMode>(kOpaqueSortModeKey);
#endif
#endif	

		return _camera;
	}
	
	#endregion
}