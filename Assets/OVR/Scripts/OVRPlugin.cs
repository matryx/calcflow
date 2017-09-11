/************************************************************************************

Copyright   :   Copyright 2014 Oculus VR, LLC. All Rights reserved.

Licensed under the Oculus VR Rift SDK License Version 3.3 (the "License");
you may not use the Oculus VR Rift SDK except in compliance with the License,
which is provided at the time of installation or download, or which
otherwise accompanies this software in either electronic or hard copy form.

You may obtain a copy of the License at

http://www.oculus.com/licenses/LICENSE-3.3

Unless required by applicable law or agreed to in writing, the Oculus VR SDK
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

************************************************************************************/

using System;
using System.Runtime.InteropServices;
using UnityEngine;

// Internal C# wrapper for OVRPlugin.

internal static class OVRPlugin
{
	public static readonly System.Version wrapperVersion = OVRP_1_16_0.version;

	private static System.Version _version;
	public static System.Version version
	{
		get {
			if (_version == null)
			{
				try
				{
					string pluginVersion = OVRP_1_1_0.ovrp_GetVersion();

					if (pluginVersion != null)
					{
						// Truncate unsupported trailing version info for System.Version. Original string is returned if not present.
						pluginVersion = pluginVersion.Split('-')[0];
						_version = new System.Version(pluginVersion);
					}
					else
					{
						_version = _versionZero;
					}
				}
				catch
				{
					_version = _versionZero;
				}

				// Unity 5.1.1f3-p3 have OVRPlugin version "0.5.0", which isn't accurate.
				if (_version == OVRP_0_5_0.version)
					_version = OVRP_0_1_0.version;

				if (_version > _versionZero && _version < OVRP_1_3_0.version)
					throw new PlatformNotSupportedException("Oculus Utilities version " + wrapperVersion + " is too new for OVRPlugin version " + _version.ToString() + ". Update to the latest version of Unity.");
			}

			return _version;
		}
	}

	private static System.Version _nativeSDKVersion;
	public static System.Version nativeSDKVersion
	{
		get {
			if (_nativeSDKVersion == null)
			{
				try
				{
					string sdkVersion = string.Empty;

					if (version >= OVRP_1_1_0.version)
						sdkVersion = OVRP_1_1_0.ovrp_GetNativeSDKVersion();
					else
						sdkVersion = _versionZero.ToString();

					if (sdkVersion != null)
					{
						// Truncate unsupported trailing version info for System.Version. Original string is returned if not present.
						sdkVersion = sdkVersion.Split('-')[0];
						_nativeSDKVersion = new System.Version(sdkVersion);
					}
					else
					{
						_nativeSDKVersion = _versionZero;
					}
				}
				catch
				{
					_nativeSDKVersion = _versionZero;
				}
			}

			return _nativeSDKVersion;
		}
	}

	[StructLayout(LayoutKind.Sequential)]
	private class GUID
	{
		public int a;
		public short b;
		public short c;
		public byte d0;
		public byte d1;
		public byte d2;
		public byte d3;
		public byte d4;
		public byte d5;
		public byte d6;
		public byte d7;
	}

	public enum Bool
	{
		False = 0,
		True
	}

	public enum Result
	{
		/// Success
		Success = 0,

		/// Failure
		Failure = -1000,
		Failure_InvalidParameter = -1001,
		Failure_NotInitialized = -1002,
		Failure_InvalidOperation = -1003,
		Failure_Unsupported = -1004,
		Failure_NotYetImplemented = -1005,
		Failure_OperationFailed = -1006,
		Failure_InsufficientSize = -1007,
	}

	public enum CameraStatus
	{
		CameraStatus_None,
		CameraStatus_Connected,
		CameraStatus_Calibrating,
		CameraStatus_CalibrationFailed,
		CameraStatus_Calibrated,
		CameraStatus_EnumSize = 0x7fffffff
	}

	public enum Eye
	{
		None  = -1,
		Left  = 0,
		Right = 1,
		Count = 2
	}

	public enum Tracker
	{
		None   = -1,
		Zero   = 0,
		One    = 1,
		Two    = 2,
		Three  = 3,
		Count,
	}

	public enum Node
	{
		None             = -1,
		EyeLeft          = 0,
		EyeRight         = 1,
		EyeCenter        = 2,
		HandLeft         = 3,
		HandRight        = 4,
		TrackerZero      = 5,
		TrackerOne       = 6,
		TrackerTwo       = 7,
		TrackerThree     = 8,
		Head             = 9,
		DeviceObjectZero = 10,
		Count,
	}

	public enum Controller
	{
		None               = 0,
		LTouch             = 0x00000001,
		RTouch             = 0x00000002,
		Touch              = LTouch | RTouch,
		Remote             = 0x00000004,
		Gamepad            = 0x00000010,
		Touchpad           = 0x08000000,
		LTrackedRemote     = 0x01000000,
		RTrackedRemote     = 0x02000000,
		Active             = unchecked((int)0x80000000),
		All                = ~None,
	}

	public enum TrackingOrigin
	{
		EyeLevel       = 0,
		FloorLevel     = 1,
		Count,
	}

	public enum RecenterFlags
	{
		Default           = 0,
		Controllers       = 0x40000000,
		IgnoreAll         = unchecked((int)0x80000000),
		Count,
	}

	public enum BatteryStatus
	{
		Charging = 0,
		Discharging,
		Full,
		NotCharging,
		Unknown,
	}

	public enum EyeTextureFormat
	{
		Default = 0,
		R8G8B8A8_sRGB = 0,
		R8G8B8A8 = 1,
		R16G16B16A16_FP = 2,
		R11G11B10_FP = 3,
		B8G8R8A8_sRGB = 4,
		B8G8R8A8 = 5,
		EnumSize = 0x7fffffff
	}

	public enum PlatformUI
	{
		None = -1,
		GlobalMenu = 0,
		ConfirmQuit,
		GlobalMenuTutorial,
	}

	public enum SystemRegion
	{
		Unspecified = 0,
		Japan,
		China,
	}

	public enum SystemHeadset
	{
		None = 0,
		GearVR_R320, // Note4 Innovator
		GearVR_R321, // S6 Innovator
		GearVR_R322, // Commercial 1
		GearVR_R323, // Commercial 2 (USB Type C)
		GearVR_R324, // Commercial 3 (USB Type C)

		Rift_DK1 = 0x1000,
		Rift_DK2,
		Rift_CV1,
	}

	public enum OverlayShape
	{
		Quad = 0,
		Cylinder = 1,
		Cubemap = 2,
		OffcenterCubemap = 4,
	}

	public enum Step
	{
		Render = -1,
		Physics = 0,
	}

	public enum CameraDevice
	{
		None = 0,
		WebCamera0 = 100,
		WebCamera1 = 101,
	}

	private const int OverlayShapeFlagShift = 4;
	private enum OverlayFlag
	{
		None        = unchecked((int)0x00000000),
		OnTop       = unchecked((int)0x00000001),
		HeadLocked  = unchecked((int)0x00000002),

		// Using the 5-8 bits for shapes, total 16 potential shapes can be supported 0x000000[0]0 ->  0x000000[F]0
		ShapeFlag_Quad      = unchecked((int)OverlayShape.Quad << OverlayShapeFlagShift),
		ShapeFlag_Cylinder  = unchecked((int)OverlayShape.Cylinder << OverlayShapeFlagShift),
		ShapeFlag_Cubemap = unchecked((int)OverlayShape.Cubemap << OverlayShapeFlagShift),
		ShapeFlag_OffcenterCubemap = unchecked((int)OverlayShape.OffcenterCubemap << OverlayShapeFlagShift),
		ShapeFlagRangeMask = unchecked((int)0xF << OverlayShapeFlagShift),
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct Vector2f
	{
		public float x;
		public float y;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct Vector3f
	{
		public float x;
		public float y;
		public float z;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct Quatf
	{
		public float x;
		public float y;
		public float z;
		public float w;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct Posef
	{
		public Quatf Orientation;
		public Vector3f Position;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct PoseStatef
	{
		public Posef Pose;
		public Vector3f Velocity;
		public Vector3f Acceleration;
		public Vector3f AngularVelocity;
		public Vector3f AngularAcceleration;
		double Time;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct ControllerState4
	{
		public uint ConnectedControllers;
		public uint Buttons;
		public uint Touches;
		public uint NearTouches;
		public float LIndexTrigger;
		public float RIndexTrigger;
		public float LHandTrigger;
		public float RHandTrigger;
		public Vector2f LThumbstick;
		public Vector2f RThumbstick;
		public Vector2f LTouchpad;
		public Vector2f RTouchpad;
		public byte LBatteryPercentRemaining;
		public byte RBatteryPercentRemaining;
		public byte LRecenterCount;
		public byte RRecenterCount;
		public byte Reserved_27;
		public byte Reserved_26;
		public byte Reserved_25;
		public byte Reserved_24;
		public byte Reserved_23;
		public byte Reserved_22;
		public byte Reserved_21;
		public byte Reserved_20;
		public byte Reserved_19;
		public byte Reserved_18;
		public byte Reserved_17;
		public byte Reserved_16;
		public byte Reserved_15;
		public byte Reserved_14;
		public byte Reserved_13;
		public byte Reserved_12;
		public byte Reserved_11;
		public byte Reserved_10;
		public byte Reserved_09;
		public byte Reserved_08;
		public byte Reserved_07;
		public byte Reserved_06;
		public byte Reserved_05;
		public byte Reserved_04;
		public byte Reserved_03;
		public byte Reserved_02;
		public byte Reserved_01;
		public byte Reserved_00;

		public ControllerState4(ControllerState2 cs)
		{
			ConnectedControllers = cs.ConnectedControllers;
			Buttons = cs.Buttons;
			Touches = cs.Touches;
			NearTouches = cs.NearTouches;
			LIndexTrigger = cs.LIndexTrigger;
			RIndexTrigger = cs.RIndexTrigger;
			LHandTrigger = cs.LHandTrigger;
			RHandTrigger = cs.RHandTrigger;
			LThumbstick = cs.LThumbstick;
			RThumbstick = cs.RThumbstick;
			LTouchpad = cs.LTouchpad;
			RTouchpad = cs.RTouchpad;
			LBatteryPercentRemaining = 0;
			RBatteryPercentRemaining = 0;
			LRecenterCount = 0;
			RRecenterCount = 0;
			Reserved_27 = 0;
			Reserved_26 = 0;
			Reserved_25 = 0;
			Reserved_24 = 0;
			Reserved_23 = 0;
			Reserved_22 = 0;
			Reserved_21 = 0;
			Reserved_20 = 0;
			Reserved_19 = 0;
			Reserved_18 = 0;
			Reserved_17 = 0;
			Reserved_16 = 0;
			Reserved_15 = 0;
			Reserved_14 = 0;
			Reserved_13 = 0;
			Reserved_12 = 0;
			Reserved_11 = 0;
			Reserved_10 = 0;
			Reserved_09 = 0;
			Reserved_08 = 0;
			Reserved_07 = 0;
			Reserved_06 = 0;
			Reserved_05 = 0;
			Reserved_04 = 0;
			Reserved_03 = 0;
			Reserved_02 = 0;
			Reserved_01 = 0;
			Reserved_00 = 0;
		}
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct ControllerState2
	{
		public uint ConnectedControllers;
		public uint Buttons;
		public uint Touches;
		public uint NearTouches;
		public float LIndexTrigger;
		public float RIndexTrigger;
		public float LHandTrigger;
		public float RHandTrigger;
		public Vector2f LThumbstick;
		public Vector2f RThumbstick;
		public Vector2f LTouchpad;
		public Vector2f RTouchpad;

		public ControllerState2(ControllerState cs)
		{
			ConnectedControllers = cs.ConnectedControllers;
			Buttons = cs.Buttons;
			Touches = cs.Touches;
			NearTouches = cs.NearTouches;
			LIndexTrigger = cs.LIndexTrigger;
			RIndexTrigger = cs.RIndexTrigger;
			LHandTrigger = cs.LHandTrigger;
			RHandTrigger = cs.RHandTrigger;
			LThumbstick = cs.LThumbstick;
			RThumbstick = cs.RThumbstick;
			LTouchpad = new Vector2f() { x = 0.0f, y = 0.0f };
			RTouchpad = new Vector2f() { x = 0.0f, y = 0.0f };
		}
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct ControllerState
	{
		public uint ConnectedControllers;
		public uint Buttons;
		public uint Touches;
		public uint NearTouches;
		public float LIndexTrigger;
		public float RIndexTrigger;
		public float LHandTrigger;
		public float RHandTrigger;
		public Vector2f LThumbstick;
		public Vector2f RThumbstick;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct HapticsBuffer
	{
		public IntPtr Samples;
		public int SamplesCount;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct HapticsState
	{
		public int SamplesAvailable;
		public int SamplesQueued;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct HapticsDesc
	{
		public int SampleRateHz;
		public int SampleSizeInBytes;
		public int MinimumSafeSamplesQueued;
		public int MinimumBufferSamplesCount;
		public int OptimalBufferSamplesCount;
		public int MaximumBufferSamplesCount;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct AppPerfFrameStats
	{
		public int HmdVsyncIndex;
		public int AppFrameIndex;
		public int AppDroppedFrameCount;
		public float AppMotionToPhotonLatency;
		public float AppQueueAheadTime;
		public float AppCpuElapsedTime;
		public float AppGpuElapsedTime;
		public int CompositorFrameIndex;
		public int CompositorDroppedFrameCount;
		public float CompositorLatency;
		public float CompositorCpuElapsedTime;
		public float CompositorGpuElapsedTime;
		public float CompositorCpuStartToGpuEndElapsedTime;
		public float CompositorGpuEndToVsyncElapsedTime;
	}

	public const int AppPerfFrameStatsMaxCount = 5;

	[StructLayout(LayoutKind.Sequential)]
	public struct AppPerfStats
	{
		[MarshalAs(UnmanagedType.ByValArray, SizeConst = AppPerfFrameStatsMaxCount)]
		public AppPerfFrameStats[] FrameStats;
		public int FrameStatsCount;
		public Bool AnyFrameStatsDropped;
		public float AdaptiveGpuPerformanceScale;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct Sizei
	{
		public int w;
		public int h;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct Sizef
	{
		public float w;
		public float h;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct Vector2i
	{
		public int x;
		public int y;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct Recti {
		Vector2i Pos;
		Sizei Size;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct Rectf {
		Vector2f Pos;
		Sizef Size;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct Frustumf
	{
		public float zNear;
		public float zFar;
		public float fovX;
		public float fovY;
	}

	public enum BoundaryType
	{
		OuterBoundary      = 0x0001,
		PlayArea           = 0x0100,
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct BoundaryTestResult
	{
		public Bool IsTriggering;
		public float ClosestDistance;
		public Vector3f ClosestPoint;
		public Vector3f ClosestPointNormal;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct BoundaryLookAndFeel
	{
		public Colorf Color;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct BoundaryGeometry
	{
		public BoundaryType BoundaryType;
		[MarshalAs(UnmanagedType.ByValArray, SizeConst = 256)]
		public Vector3f[] Points;
		public int PointsCount;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct Colorf
	{
		public float r;
		public float g;
		public float b;
		public float a;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct Fovf
	{
		public float UpTan;
		public float DownTan;
		public float LeftTan;
		public float RightTan;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct CameraIntrinsics
	{
		public bool IsValid;
		public double LastChangedTimeSeconds;
		public Fovf FOVPort;
		public float VirtualNearPlaneDistanceMeters;
		public float VirtualFarPlaneDistanceMeters;
		public Sizei ImageSensorPixelResolution;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct CameraExtrinsics
	{
		public bool IsValid;
		public double LastChangedTimeSeconds;
		public CameraStatus CameraStatusData;
		public Node AttachedToNode;
		public Posef RelativePose;
	}

	public enum LayerLayout
	{
		Stereo = 0,
		Mono = 1,
		DoubleWide = 2,
		Array = 3,
		EnumSize = 0xF
	}

	public enum LayerFlags
	{
		Static = (1 << 0),
		LoadingScreen = (1 << 1),
		SymmetricFov = (1 << 2),
		TextureOriginAtBottomLeft = (1 << 3),
		ChromaticAberrationCorrection = (1 << 4),
		NoAllocation = (1 << 5),
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct LayerDesc
	{
		public OverlayShape Shape;
		public LayerLayout Layout;
		public Sizei TextureSize;
		public int MipLevels;
		public int SampleCount;
		public EyeTextureFormat Format;
		public int LayerFlags;

		//Eye FOV-only members.
		[MarshalAs(UnmanagedType.ByValArray, SizeConst=2)]
		public Fovf[] Fov;
		[MarshalAs(UnmanagedType.ByValArray, SizeConst=2)]
		public Rectf[] VisibleRect;
		public Sizei MaxViewportSize;
	}

	[StructLayout(LayoutKind.Sequential)]
	public struct LayerSubmit
	{
		int LayerId;
		int TextureStage;
		[MarshalAs(UnmanagedType.ByValArray, SizeConst=2)]
		Recti[] ViewportRect;
		Posef Pose;
		int LayerSubmitFlags;
	}

	public static bool initialized
	{
		get {
			return OVRP_1_1_0.ovrp_GetInitialized() == OVRPlugin.Bool.True;
		}
	}

	public static bool chromatic
	{
		get {
			if (version >= OVRP_1_7_0.version)
				return OVRP_1_7_0.ovrp_GetAppChromaticCorrection() == OVRPlugin.Bool.True;

#if UNITY_ANDROID && !UNITY_EDITOR
			return false;
#else
			return true;
#endif
		}

		set {
			if (version >= OVRP_1_7_0.version)
				OVRP_1_7_0.ovrp_SetAppChromaticCorrection(ToBool(value));
		}
	}

	public static bool monoscopic
	{
		get { return OVRP_1_1_0.ovrp_GetAppMonoscopic() == OVRPlugin.Bool.True; }
		set { OVRP_1_1_0.ovrp_SetAppMonoscopic(ToBool(value)); }
	}

	public static bool rotation
	{
		get { return OVRP_1_1_0.ovrp_GetTrackingOrientationEnabled() == Bool.True; }
		set { OVRP_1_1_0.ovrp_SetTrackingOrientationEnabled(ToBool(value)); }
	}

	public static bool position
	{
		get { return OVRP_1_1_0.ovrp_GetTrackingPositionEnabled() == Bool.True; }
		set { OVRP_1_1_0.ovrp_SetTrackingPositionEnabled(ToBool(value)); }
	}

	public static bool useIPDInPositionTracking
	{
		get {
			if (version >= OVRP_1_6_0.version)
				return OVRP_1_6_0.ovrp_GetTrackingIPDEnabled() == OVRPlugin.Bool.True;

			return true;
		}

		set {
			if (version >= OVRP_1_6_0.version)
				OVRP_1_6_0.ovrp_SetTrackingIPDEnabled(ToBool(value));
		}
	}

	public static bool positionSupported { get { return OVRP_1_1_0.ovrp_GetTrackingPositionSupported() == Bool.True; } }

	public static bool positionTracked { get { return OVRP_1_1_0.ovrp_GetNodePositionTracked(Node.EyeCenter) == Bool.True; } }

	public static bool powerSaving { get { return OVRP_1_1_0.ovrp_GetSystemPowerSavingMode() == Bool.True; } }

	public static bool hmdPresent { get { return OVRP_1_1_0.ovrp_GetNodePresent(Node.EyeCenter) == Bool.True; } }

	public static bool userPresent { get { return OVRP_1_1_0.ovrp_GetUserPresent() == Bool.True; } }

	public static bool headphonesPresent { get { return OVRP_1_3_0.ovrp_GetSystemHeadphonesPresent() == OVRPlugin.Bool.True; } }

	public static int recommendedMSAALevel
	{
		get {
			if (version >= OVRP_1_6_0.version)
				return OVRP_1_6_0.ovrp_GetSystemRecommendedMSAALevel();
			else
				return 2;
		}
	}

	public static SystemRegion systemRegion
	{
		get {
			if (version >= OVRP_1_5_0.version)
				return OVRP_1_5_0.ovrp_GetSystemRegion();
			else
				return SystemRegion.Unspecified;
		}
	}

	private static GUID _nativeAudioOutGuid = new OVRPlugin.GUID();
	private static Guid _cachedAudioOutGuid;
	private static string _cachedAudioOutString;
	public static string audioOutId
	{
		get {
			try
			{
				if (_nativeAudioOutGuid == null)
					_nativeAudioOutGuid = new OVRPlugin.GUID();

				IntPtr ptr = OVRP_1_1_0.ovrp_GetAudioOutId();
				if (ptr != IntPtr.Zero)
				{
					Marshal.PtrToStructure(ptr, _nativeAudioOutGuid);
					Guid managedGuid = new Guid(
						_nativeAudioOutGuid.a,
						_nativeAudioOutGuid.b,
						_nativeAudioOutGuid.c,
						_nativeAudioOutGuid.d0,
						_nativeAudioOutGuid.d1,
						_nativeAudioOutGuid.d2,
						_nativeAudioOutGuid.d3,
						_nativeAudioOutGuid.d4,
						_nativeAudioOutGuid.d5,
						_nativeAudioOutGuid.d6,
						_nativeAudioOutGuid.d7);

					if (managedGuid != _cachedAudioOutGuid)
					{
						_cachedAudioOutGuid = managedGuid;
						_cachedAudioOutString = _cachedAudioOutGuid.ToString();
					}

					return _cachedAudioOutString;
				}
			}
			catch { }

			return string.Empty;
		}
	}

	private static GUID _nativeAudioInGuid = new OVRPlugin.GUID();
	private static Guid _cachedAudioInGuid;
	private static string _cachedAudioInString;
	public static string audioInId
	{
		get {
			try
			{
				if (_nativeAudioInGuid == null)
					_nativeAudioInGuid = new OVRPlugin.GUID();

				IntPtr ptr = OVRP_1_1_0.ovrp_GetAudioInId();
				if (ptr != IntPtr.Zero)
				{
					Marshal.PtrToStructure(ptr, _nativeAudioInGuid);
					Guid managedGuid = new Guid(
						_nativeAudioInGuid.a,
						_nativeAudioInGuid.b,
						_nativeAudioInGuid.c,
						_nativeAudioInGuid.d0,
						_nativeAudioInGuid.d1,
						_nativeAudioInGuid.d2,
						_nativeAudioInGuid.d3,
						_nativeAudioInGuid.d4,
						_nativeAudioInGuid.d5,
						_nativeAudioInGuid.d6,
						_nativeAudioInGuid.d7);

					if (managedGuid != _cachedAudioInGuid)
					{
						_cachedAudioInGuid = managedGuid;
						_cachedAudioInString = _cachedAudioInGuid.ToString();
					}

					return _cachedAudioInString;
				}
			}
			catch { }

			return string.Empty;
		}
	}

	public static bool hasVrFocus { get { return OVRP_1_1_0.ovrp_GetAppHasVrFocus() == Bool.True; } }

	public static bool shouldQuit { get { return OVRP_1_1_0.ovrp_GetAppShouldQuit() == Bool.True; } }

	public static bool shouldRecenter { get { return OVRP_1_1_0.ovrp_GetAppShouldRecenter() == Bool.True; } }

	public static string productName { get { return OVRP_1_1_0.ovrp_GetSystemProductName(); } }

	public static string latency { get { return OVRP_1_1_0.ovrp_GetAppLatencyTimings(); } }

	public static float eyeDepth
	{
		get { return OVRP_1_1_0.ovrp_GetUserEyeDepth(); }
		set { OVRP_1_1_0.ovrp_SetUserEyeDepth(value); }
	}

	public static float eyeHeight
	{
		get { return OVRP_1_1_0.ovrp_GetUserEyeHeight(); }
		set { OVRP_1_1_0.ovrp_SetUserEyeHeight(value); }
	}

	public static float batteryLevel
	{
		get { return OVRP_1_1_0.ovrp_GetSystemBatteryLevel(); }
	}

	public static float batteryTemperature
	{
		get { return OVRP_1_1_0.ovrp_GetSystemBatteryTemperature(); }
	}

	public static int cpuLevel
	{
		get { return OVRP_1_1_0.ovrp_GetSystemCpuLevel(); }
		set { OVRP_1_1_0.ovrp_SetSystemCpuLevel(value); }
	}

	public static int gpuLevel
	{
		get { return OVRP_1_1_0.ovrp_GetSystemGpuLevel(); }
		set { OVRP_1_1_0.ovrp_SetSystemGpuLevel(value); }
	}

	public static int vsyncCount
	{
		get { return OVRP_1_1_0.ovrp_GetSystemVSyncCount(); }
		set { OVRP_1_2_0.ovrp_SetSystemVSyncCount(value); }
	}

	public static float systemVolume
	{
		get { return OVRP_1_1_0.ovrp_GetSystemVolume(); }
	}

	public static float ipd
	{
		get { return OVRP_1_1_0.ovrp_GetUserIPD(); }
		set { OVRP_1_1_0.ovrp_SetUserIPD(value); }
	}

	public static bool occlusionMesh
	{
		get { return OVRP_1_3_0.ovrp_GetEyeOcclusionMeshEnabled() == Bool.True; }
		set { OVRP_1_3_0.ovrp_SetEyeOcclusionMeshEnabled(ToBool(value)); }
	}

	public static BatteryStatus batteryStatus
	{
		get { return OVRP_1_1_0.ovrp_GetSystemBatteryStatus(); }
	}

	public static Frustumf GetEyeFrustum(Eye eyeId) { return OVRP_1_1_0.ovrp_GetNodeFrustum((Node)eyeId); }
	public static Sizei GetEyeTextureSize(Eye eyeId) { return OVRP_0_1_0.ovrp_GetEyeTextureSize(eyeId); }
	public static Posef GetTrackerPose(Tracker trackerId) { return GetNodePose((Node)((int)trackerId + (int)Node.TrackerZero), Step.Render); }
	public static Frustumf GetTrackerFrustum(Tracker trackerId) { return OVRP_1_1_0.ovrp_GetNodeFrustum((Node)((int)trackerId + (int)Node.TrackerZero)); }
	public static bool ShowUI(PlatformUI ui) { return OVRP_1_1_0.ovrp_ShowSystemUI(ui) == Bool.True; }
	public static bool EnqueueSubmitLayer(bool onTop, bool headLocked, IntPtr leftTexture, IntPtr rightTexture, int layerId, int frameIndex, Posef pose, Vector3f scale, int layerIndex=0, OverlayShape shape=OverlayShape.Quad)
	{
		if (version >= OVRP_1_6_0.version)
		{
			uint flags = (uint)OverlayFlag.None;
			if (onTop)
				flags |= (uint)OverlayFlag.OnTop;
			if (headLocked)
				flags |= (uint)OverlayFlag.HeadLocked;

			if (shape == OverlayShape.Cylinder || shape == OverlayShape.Cubemap)
			{
#if UNITY_ANDROID
				if (version >= OVRP_1_7_0.version)
					flags |= (uint)(shape) << OverlayShapeFlagShift;
				else
#else
				if (shape == OverlayShape.Cubemap && version >= OVRP_1_10_0.version && version < OVRP_1_16_0.version)
					flags |= (uint)(shape) << OverlayShapeFlagShift;
				else if (shape == OverlayShape.Cylinder && version >= OVRP_1_16_0.version)
					flags |= (uint)(shape) << OverlayShapeFlagShift;
				else
#endif
					return false;
			}

			if (shape == OverlayShape.OffcenterCubemap)
			{
#if UNITY_ANDROID
				if (version >= OVRP_1_11_0.version)
					flags |= (uint)(shape) << OverlayShapeFlagShift;
				else
#endif
				return false;
			}

			if (version >= OVRP_1_15_0.version && layerId != -1)
				return OVRP_1_15_0.ovrp_EnqueueSubmitLayer(flags, leftTexture, rightTexture, layerId, frameIndex, pose, scale, layerIndex) == Result.Success;

			return OVRP_1_6_0.ovrp_SetOverlayQuad3(flags, leftTexture, rightTexture, IntPtr.Zero, pose, scale, layerIndex) == Bool.True;
		}

		if (layerIndex != 0)
			return false;

		return OVRP_0_1_1.ovrp_SetOverlayQuad2(ToBool(onTop), ToBool(headLocked), leftTexture, IntPtr.Zero, pose, scale) == Bool.True;
	}

	public static LayerDesc CalculateLayerDesc(OverlayShape shape, LayerLayout layout, Sizei textureSize,
		int mipLevels, int sampleCount, EyeTextureFormat format, int layerFlags)
	{
		LayerDesc layerDesc = new LayerDesc();

		if (version >= OVRP_1_15_0.version)
		{
			OVRP_1_15_0.ovrp_CalculateLayerDesc (shape, layout, ref textureSize,
				mipLevels, sampleCount, format, layerFlags, ref layerDesc);
		}

		return layerDesc;
	}

	public static bool EnqueueSetupLayer(LayerDesc desc, IntPtr layerID)
	{
		if (version >= OVRP_1_15_0.version)
			return OVRP_1_15_0.ovrp_EnqueueSetupLayer(ref desc, layerID) == Result.Success;

		return false;
	}

	public static bool EnqueueDestroyLayer(IntPtr layerID)
	{
		if (version >= OVRP_1_15_0.version)
			return OVRP_1_15_0.ovrp_EnqueueDestroyLayer(layerID) == Result.Success;

		return false;
	}

	public static IntPtr GetLayerTexture(int layerId, int stage, Eye eyeId)
	{
		IntPtr textureHandle = IntPtr.Zero;

		if (version >= OVRP_1_15_0.version)
			OVRP_1_15_0.ovrp_GetLayerTexturePtr(layerId, stage, eyeId, ref textureHandle);

		return textureHandle;
	}

	public static int GetLayerTextureStageCount(int layerId)
	{
		int stageCount = 1;

		if (version >= OVRP_1_15_0.version)
			OVRP_1_15_0.ovrp_GetLayerTextureStageCount(layerId, ref stageCount);

		return stageCount;
	}

	public static bool UpdateNodePhysicsPoses(int frameIndex, double predictionSeconds)
	{
		if (version >= OVRP_1_8_0.version)
			return OVRP_1_8_0.ovrp_Update2((int)Step.Physics, frameIndex, predictionSeconds) == Bool.True;

		return false;
	}

	public static Posef GetNodePose(Node nodeId, Step stepId)
	{
		if (version >= OVRP_1_12_0.version)
			return OVRP_1_12_0.ovrp_GetNodePoseState(stepId, nodeId).Pose;

		if (version >= OVRP_1_8_0.version && stepId == Step.Physics)
			return OVRP_1_8_0.ovrp_GetNodePose2(0, nodeId);

		return OVRP_0_1_2.ovrp_GetNodePose(nodeId);
	}

	public static Vector3f GetNodeVelocity(Node nodeId, Step stepId)
	{
		if (version >= OVRP_1_12_0.version)
			return OVRP_1_12_0.ovrp_GetNodePoseState(stepId, nodeId).Velocity;

		if (version >= OVRP_1_8_0.version && stepId == Step.Physics)
			return OVRP_1_8_0.ovrp_GetNodeVelocity2(0, nodeId).Position;

		return OVRP_0_1_3.ovrp_GetNodeVelocity(nodeId).Position;
	}

	public static Vector3f GetNodeAngularVelocity(Node nodeId, Step stepId)
	{
		if (version >= OVRP_1_12_0.version)
			return OVRP_1_12_0.ovrp_GetNodePoseState(stepId, nodeId).AngularVelocity;

		return new Vector3f(); //TODO: Convert legacy quat to vec3?
	}

	public static Vector3f GetNodeAcceleration(Node nodeId, Step stepId)
	{
		if (version >= OVRP_1_12_0.version)
			return OVRP_1_12_0.ovrp_GetNodePoseState(stepId, nodeId).Acceleration;

		if (version >= OVRP_1_8_0.version && stepId == Step.Physics)
			return OVRP_1_8_0.ovrp_GetNodeAcceleration2(0, nodeId).Position;

		return OVRP_0_1_3.ovrp_GetNodeAcceleration(nodeId).Position;
	}

	public static Vector3f GetNodeAngularAcceleration(Node nodeId, Step stepId)
	{
		if (version >= OVRP_1_12_0.version)
			return OVRP_1_12_0.ovrp_GetNodePoseState(stepId, nodeId).AngularAcceleration;

		return new Vector3f(); //TODO: Convert legacy quat to vec3?
	}

	public static bool GetNodePresent(Node nodeId)
	{
		return OVRP_1_1_0.ovrp_GetNodePresent(nodeId) == Bool.True;
	}

	public static bool GetNodeOrientationTracked(Node nodeId)
	{
		return OVRP_1_1_0.ovrp_GetNodeOrientationTracked(nodeId) == Bool.True;
	}

	public static bool GetNodePositionTracked(Node nodeId)
	{
		return OVRP_1_1_0.ovrp_GetNodePositionTracked(nodeId) == Bool.True;
	}

	public static ControllerState GetControllerState(uint controllerMask)
	{
		return OVRP_1_1_0.ovrp_GetControllerState(controllerMask);
	}

	public static ControllerState2 GetControllerState2(uint controllerMask)
	{
		if (version >= OVRP_1_12_0.version)
		{
			return OVRP_1_12_0.ovrp_GetControllerState2(controllerMask);
		}

		return new ControllerState2(OVRP_1_1_0.ovrp_GetControllerState(controllerMask));
	}

	public static ControllerState4 GetControllerState4(uint controllerMask)
	{
		if (version >= OVRP_1_16_0.version)
		{
			ControllerState4 controllerState = new ControllerState4();
			OVRP_1_16_0.ovrp_GetControllerState4(controllerMask, ref controllerState);
			return controllerState;
		}

		return new ControllerState4(GetControllerState2(controllerMask));
	}

	public static bool SetControllerVibration(uint controllerMask, float frequency, float amplitude)
	{
		return OVRP_0_1_2.ovrp_SetControllerVibration(controllerMask, frequency, amplitude) == Bool.True;
	}

	public static HapticsDesc GetControllerHapticsDesc(uint controllerMask)
	{
		if (version >= OVRP_1_6_0.version)
		{
			return OVRP_1_6_0.ovrp_GetControllerHapticsDesc(controllerMask);
		}
		else
		{
			return new HapticsDesc();
		}
	}

	public static HapticsState GetControllerHapticsState(uint controllerMask)
	{
		if (version >= OVRP_1_6_0.version)
		{
			return OVRP_1_6_0.ovrp_GetControllerHapticsState(controllerMask);
		}
		else
		{
			return new HapticsState();
		}
	}

	public static bool SetControllerHaptics(uint controllerMask, HapticsBuffer hapticsBuffer)
	{
		if (version >= OVRP_1_6_0.version)
		{
			return OVRP_1_6_0.ovrp_SetControllerHaptics(controllerMask, hapticsBuffer) == Bool.True;
		}
		else
		{
			return false;
		}
	}

	public static float GetEyeRecommendedResolutionScale()
	{
		if (version >= OVRP_1_6_0.version)
		{
			return OVRP_1_6_0.ovrp_GetEyeRecommendedResolutionScale();
		}
		else
		{
			return 1.0f;
		}
	}

	public static float GetAppCpuStartToGpuEndTime()
	{
		if (version >= OVRP_1_6_0.version)
		{
			return OVRP_1_6_0.ovrp_GetAppCpuStartToGpuEndTime();
		}
		else
		{
			return 0.0f;
		}
	}

	public static bool GetBoundaryConfigured()
	{
		if (version >= OVRP_1_8_0.version)
		{
			return OVRP_1_8_0.ovrp_GetBoundaryConfigured() == OVRPlugin.Bool.True;
		}
		else
		{
			return false;
		}
	}

	public static BoundaryTestResult TestBoundaryNode(Node nodeId, BoundaryType boundaryType)
	{
		if (version >= OVRP_1_8_0.version)
		{
			return OVRP_1_8_0.ovrp_TestBoundaryNode(nodeId, boundaryType);
		}
		else
		{
			return new BoundaryTestResult();
		}
	}

	public static BoundaryTestResult TestBoundaryPoint(Vector3f point, BoundaryType boundaryType)
	{
		if (version >= OVRP_1_8_0.version)
		{
			return OVRP_1_8_0.ovrp_TestBoundaryPoint(point, boundaryType);
		}
		else
		{
			return new BoundaryTestResult();
		}
	}

	public static bool SetBoundaryLookAndFeel(BoundaryLookAndFeel lookAndFeel)
	{
		if (version >= OVRP_1_8_0.version)
		{
			return OVRP_1_8_0.ovrp_SetBoundaryLookAndFeel(lookAndFeel) == OVRPlugin.Bool.True;
		}
		else
		{
			return false;
		}
	}

	public static bool ResetBoundaryLookAndFeel()
	{
		if (version >= OVRP_1_8_0.version)
		{
			return OVRP_1_8_0.ovrp_ResetBoundaryLookAndFeel() == OVRPlugin.Bool.True;
		}
		else
		{
			return false;
		}
	}

	public static BoundaryGeometry GetBoundaryGeometry(BoundaryType boundaryType)
	{
		if (version >= OVRP_1_8_0.version)
		{
			return OVRP_1_8_0.ovrp_GetBoundaryGeometry(boundaryType);
		}
		else
		{
			return new BoundaryGeometry();
		}
	}

	public static bool GetBoundaryGeometry2(BoundaryType boundaryType, IntPtr points, ref int pointsCount)
	{
		if (version >= OVRP_1_9_0.version)
		{
			return OVRP_1_9_0.ovrp_GetBoundaryGeometry2(boundaryType, points, ref pointsCount) == OVRPlugin.Bool.True;
		}
		else
		{
			pointsCount = 0;

			return false;
		}
	}

	public static AppPerfStats GetAppPerfStats()
	{
		if (version >= OVRP_1_9_0.version)
		{
			return OVRP_1_9_0.ovrp_GetAppPerfStats();
		}
		else
		{
			return new AppPerfStats();
		}
	}

	public static bool ResetAppPerfStats()
	{
		if (version >= OVRP_1_9_0.version)
		{
			return OVRP_1_9_0.ovrp_ResetAppPerfStats() == OVRPlugin.Bool.True;
		}
		else
		{
			return false;
		}
	}

	public static float GetAppFramerate()
	{
		if (version >= OVRP_1_12_0.version)
		{
			return OVRP_1_12_0.ovrp_GetAppFramerate();
		}
		else
		{
			return 0.0f;
		}
	}

	public static EyeTextureFormat GetDesiredEyeTextureFormat()
	{
		if (version >= OVRP_1_11_0.version)
		{
			uint eyeTextureFormatValue = (uint)OVRP_1_11_0.ovrp_GetDesiredEyeTextureFormat();

			// convert both R8G8B8A8 and R8G8B8A8_SRGB to R8G8B8A8 here for avoid confusing developers
			if (eyeTextureFormatValue == 1)
				eyeTextureFormatValue = 0;

			return (EyeTextureFormat)eyeTextureFormatValue;
		}
		else
		{
			return EyeTextureFormat.Default;
		}
	}

	public static bool SetDesiredEyeTextureFormat(EyeTextureFormat value)
	{
		if (version >= OVRP_1_11_0.version)
		{
			return OVRP_1_11_0.ovrp_SetDesiredEyeTextureFormat(value) == OVRPlugin.Bool.True;
		}
		else
		{
			return false;
		}
	}

	public static bool InitializeMixedReality()
	{
		if (version >= OVRP_1_15_0.version)
		{
			Result result = OVRP_1_15_0.ovrp_InitializeMixedReality();
			if (result != Result.Success)
				Debug.LogWarning("ovrp_InitializeMixedReality return " + result);
			return result == Result.Success;
		}
		else
		{
			return false;
		}
	}

	public static bool ShutdownMixedReality()
	{
		if (version >= OVRP_1_15_0.version)
		{
			Result result = OVRP_1_15_0.ovrp_ShutdownMixedReality();
			if (result != Result.Success)
				Debug.LogWarning("ovrp_ShutdownMixedReality return " + result);
			return result == Result.Success;
		}
		else
		{
			return false;
		}
	}

	public static bool IsMixedRealityInitialized()
	{
		if (version >= OVRP_1_15_0.version)
		{
			return OVRP_1_15_0.ovrp_GetMixedRealityInitialized() == Bool.True;
		}
		else
		{
			return false;
		}
	}

	public static int GetExternalCameraCount()
	{
		if (version >= OVRP_1_15_0.version)
		{
			int cameraCount = 0;
			Result result = OVRP_1_15_0.ovrp_GetExternalCameraCount(out cameraCount);
			if (result != OVRPlugin.Result.Success)
			{
				Debug.LogWarning("ovrp_GetExternalCameraCount return " + result);
				return 0;
			}

			return cameraCount;
		}
		else
		{
			return 0;
		}
	}

	public static bool UpdateExternalCamera()
	{
		if (version >= OVRP_1_15_0.version)
		{
			Result result = OVRP_1_15_0.ovrp_UpdateExternalCamera();
			if (result != Result.Success)
				Debug.LogWarning("ovrp_UpdateExternalCamera return " + result);
			return result == Result.Success;
		}
		else
		{
			return false;
		}
	}

	public static bool GetMixedRealityCameraInfo(int cameraId, out CameraExtrinsics cameraExtrinsics, out CameraIntrinsics cameraIntrinsics)
	{
		cameraExtrinsics = default(CameraExtrinsics);
		cameraIntrinsics = default(CameraIntrinsics);

		if (version >= OVRP_1_15_0.version)
		{
			bool retValue = true;

			Result result = OVRP_1_15_0.ovrp_GetExternalCameraExtrinsics(cameraId, out cameraExtrinsics);
			if (result != Result.Success)
			{
				retValue = false;
				Debug.LogWarning("ovrp_GetExternalCameraExtrinsics return " + result);
			}

			result = OVRP_1_15_0.ovrp_GetExternalCameraIntrinsics(cameraId, out cameraIntrinsics);
			if (result != Result.Success)
			{
				retValue = false;
				Debug.LogWarning("ovrp_GetExternalCameraIntrinsics return " + result);
			}
			return retValue;
		}
		else
		{
			return false;
		}
	}

	public static Vector3f GetBoundaryDimensions(BoundaryType boundaryType)
	{
		if (version >= OVRP_1_8_0.version)
		{
			return OVRP_1_8_0.ovrp_GetBoundaryDimensions(boundaryType);
		}
		else
		{
			return new Vector3f();
		}
	}

	public static bool GetBoundaryVisible()
	{
		if (version >= OVRP_1_8_0.version)
		{
			return OVRP_1_8_0.ovrp_GetBoundaryVisible() == OVRPlugin.Bool.True;
		}
		else
		{
			return false;
		}
	}

	public static bool SetBoundaryVisible(bool value)
	{
		if (version >= OVRP_1_8_0.version)
		{
			return OVRP_1_8_0.ovrp_SetBoundaryVisible(ToBool(value)) == OVRPlugin.Bool.True;
		}
		else
		{
			return false;
		}
	}

	public static SystemHeadset GetSystemHeadsetType()
	{
		if (version >= OVRP_1_9_0.version)
			return OVRP_1_9_0.ovrp_GetSystemHeadsetType();

		return SystemHeadset.None;
	}

	public static Controller GetActiveController()
	{
		if (version >= OVRP_1_9_0.version)
			return OVRP_1_9_0.ovrp_GetActiveController();

		return Controller.None;
	}

	public static Controller GetConnectedControllers()
	{
		if (version >= OVRP_1_9_0.version)
			return OVRP_1_9_0.ovrp_GetConnectedControllers();

		return Controller.None;
	}

	private static Bool ToBool(bool b)
	{
		return (b) ? OVRPlugin.Bool.True : OVRPlugin.Bool.False;
	}

	public static TrackingOrigin GetTrackingOriginType()
	{
		return OVRP_1_0_0.ovrp_GetTrackingOriginType();
	}

	public static bool SetTrackingOriginType(TrackingOrigin originType)
	{
		return OVRP_1_0_0.ovrp_SetTrackingOriginType(originType) == Bool.True;
	}

	public static Posef GetTrackingCalibratedOrigin()
	{
		return OVRP_1_0_0.ovrp_GetTrackingCalibratedOrigin();
	}

	public static bool SetTrackingCalibratedOrigin()
	{
		return OVRP_1_2_0.ovrpi_SetTrackingCalibratedOrigin() == Bool.True;
	}

	public static bool RecenterTrackingOrigin(RecenterFlags flags)
	{
		return OVRP_1_0_0.ovrp_RecenterTrackingOrigin((uint)flags) == Bool.True;
	}

	public static bool UpdateCameraDevices()
	{
		if (version >= OVRP_1_16_0.version)
		{
			Result result = OVRP_1_16_0.ovrp_UpdateCameraDevices();
			if (result != Result.Success)
				Debug.LogWarning("ovrp_UpdateCameraDevices return " + result);
			return result == Result.Success;
		}
		else
		{
			return false;
		}
	}

	public static bool IsCameraDeviceAvailable(CameraDevice cameraDevice)
	{
		if (version >= OVRP_1_16_0.version)
		{
			Bool result = OVRP_1_16_0.ovrp_IsCameraDeviceAvailable(cameraDevice);
			return result == Bool.True;
		}
		else
		{
			return false;
		}
	}

	public static bool SetCameraDevicePreferredColorFrameSize(CameraDevice cameraDevice, int width, int height)
	{
		if (version >= OVRP_1_16_0.version)
		{
			Sizei size = new Sizei();
			size.w = width;
			size.h = height;
			Result result = OVRP_1_16_0.ovrp_SetCameraDevicePreferredColorFrameSize(cameraDevice, size);
			if (result != Result.Success)
				Debug.LogWarning("ovrp_SetCameraDevicePreferredColorFrameSize return " + result);
			return result == Result.Success;
		}
		else
		{
			return false;
		}
	}

	public static bool OpenCameraDevice(CameraDevice cameraDevice)
	{
		if (version >= OVRP_1_16_0.version)
		{
			Result result = OVRP_1_16_0.ovrp_OpenCameraDevice(cameraDevice);
			if (result != Result.Success)
				Debug.LogWarning("ovrp_OpenCameraDevice return " + result);
			return result == Result.Success;
		}
		else
		{
			return false;
		}
	}

	public static bool CloseCameraDevice(CameraDevice cameraDevice)
	{
		if (version >= OVRP_1_16_0.version)
		{
			Result result = OVRP_1_16_0.ovrp_CloseCameraDevice(cameraDevice);
			if (result != Result.Success)
				Debug.LogWarning("ovrp_OpenCameraDevice return " + result);
			return result == Result.Success;
		}
		else
		{
			return false;
		}
	}

	public static bool HasCameraDeviceOpened(CameraDevice cameraDevice)
	{
		if (version >= OVRP_1_16_0.version)
		{
			Bool result = OVRP_1_16_0.ovrp_HasCameraDeviceOpened(cameraDevice);
			return result == Bool.True;
		}
		else
		{
			return false;
		}
	}

	public static bool IsCameraDeviceColorFrameAvailable(CameraDevice cameraDevice)
	{
		if (version >= OVRP_1_16_0.version)
		{
			Bool result = OVRP_1_16_0.ovrp_IsCameraDeviceColorFrameAvailable(cameraDevice);
			return result == Bool.True;
		}
		else
		{
			return false;
		}
	}

	private static Texture2D cachedCameraFrameTexture = null;
	public static Texture2D GetCameraDeviceColorFrameTexture(CameraDevice cameraDevice)
	{
		if (version >= OVRP_1_16_0.version)
		{
			Sizei size = new Sizei();
			Result result = OVRP_1_16_0.ovrp_GetCameraDeviceColorFrameSize(cameraDevice, out size);
			if (result != Result.Success)
			{
				Debug.LogWarning("ovrp_SetCameraDevicePreferredColorFrameSize return " + result);
				return null;
			}
			IntPtr pixels;
			int rowPitch;
			result = OVRP_1_16_0.ovrp_GetCameraDeviceColorFrameBgraPixels(cameraDevice, out pixels, out rowPitch);
			if (result != Result.Success)
			{
				Debug.LogWarning("ovrp_GetCameraDeviceColorFrameBgraPixels return " + result);
				return null;
			}
			if (rowPitch != size.w * 4)
			{
				Debug.LogWarning(string.Format("RowPitch mismatch, expected {0}, get {1}", size.w * 4, rowPitch));
				return null;
			}
			if (!cachedCameraFrameTexture || cachedCameraFrameTexture.width != size.w || cachedCameraFrameTexture.height != size.h)
			{
				cachedCameraFrameTexture = new Texture2D(size.w, size.h, TextureFormat.BGRA32, false);
			}
			cachedCameraFrameTexture.LoadRawTextureData(pixels, rowPitch * size.h);
			cachedCameraFrameTexture.Apply();
			return cachedCameraFrameTexture;
		}
		else
		{
			return null;
		}
	}

	private const string pluginName = "OVRPlugin";
	private static Version _versionZero = new System.Version(0, 0, 0);

	private static class OVRP_0_1_0
	{
		public static readonly System.Version version = new System.Version(0, 1, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Sizei ovrp_GetEyeTextureSize(Eye eyeId);
	}

	private static class OVRP_0_1_1
	{
		public static readonly System.Version version = new System.Version(0, 1, 1);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetOverlayQuad2(Bool onTop, Bool headLocked, IntPtr texture, IntPtr device, Posef pose, Vector3f scale);
	}

	private static class OVRP_0_1_2
	{
		public static readonly System.Version version = new System.Version(0, 1, 2);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Posef ovrp_GetNodePose(Node nodeId);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetControllerVibration(uint controllerMask, float frequency, float amplitude);
	}

	private static class OVRP_0_1_3
	{
		public static readonly System.Version version = new System.Version(0, 1, 3);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Posef ovrp_GetNodeVelocity(Node nodeId);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Posef ovrp_GetNodeAcceleration(Node nodeId);
	}

	private static class OVRP_0_5_0
	{
		public static readonly System.Version version = new System.Version(0, 5, 0);
	}

	private static class OVRP_1_0_0
	{
		public static readonly System.Version version = new System.Version(1, 0, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern TrackingOrigin ovrp_GetTrackingOriginType();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetTrackingOriginType(TrackingOrigin originType);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Posef ovrp_GetTrackingCalibratedOrigin();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_RecenterTrackingOrigin(uint flags);
	}

	private static class OVRP_1_1_0
	{
		public static readonly System.Version version = new System.Version(1, 1, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetInitialized();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "ovrp_GetVersion")]
		private static extern IntPtr _ovrp_GetVersion();
		public static string ovrp_GetVersion() { return Marshal.PtrToStringAnsi(_ovrp_GetVersion()); }

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "ovrp_GetNativeSDKVersion")]
		private static extern IntPtr _ovrp_GetNativeSDKVersion();
		public static string ovrp_GetNativeSDKVersion() { return Marshal.PtrToStringAnsi(_ovrp_GetNativeSDKVersion()); }

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern IntPtr ovrp_GetAudioOutId();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern IntPtr ovrp_GetAudioInId();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern float ovrp_GetEyeTextureScale();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetEyeTextureScale(float value);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetTrackingOrientationSupported();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetTrackingOrientationEnabled();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetTrackingOrientationEnabled(Bool value);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetTrackingPositionSupported();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetTrackingPositionEnabled();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetTrackingPositionEnabled(Bool value);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetNodePresent(Node nodeId);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetNodeOrientationTracked(Node nodeId);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetNodePositionTracked(Node nodeId);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Frustumf ovrp_GetNodeFrustum(Node nodeId);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern ControllerState ovrp_GetControllerState(uint controllerMask);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern int ovrp_GetSystemCpuLevel();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetSystemCpuLevel(int value);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern int ovrp_GetSystemGpuLevel();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetSystemGpuLevel(int value);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetSystemPowerSavingMode();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern float ovrp_GetSystemDisplayFrequency();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern int ovrp_GetSystemVSyncCount();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern float ovrp_GetSystemVolume();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern BatteryStatus ovrp_GetSystemBatteryStatus();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern float ovrp_GetSystemBatteryLevel();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern float ovrp_GetSystemBatteryTemperature();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "ovrp_GetSystemProductName")]
		private static extern IntPtr _ovrp_GetSystemProductName();
		public static string ovrp_GetSystemProductName() { return Marshal.PtrToStringAnsi(_ovrp_GetSystemProductName()); }

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_ShowSystemUI(PlatformUI ui);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetAppMonoscopic();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetAppMonoscopic(Bool value);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetAppHasVrFocus();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetAppShouldQuit();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetAppShouldRecenter();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "ovrp_GetAppLatencyTimings")]
		private static extern IntPtr _ovrp_GetAppLatencyTimings();
		public static string ovrp_GetAppLatencyTimings() { return Marshal.PtrToStringAnsi(_ovrp_GetAppLatencyTimings()); }

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetUserPresent();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern float ovrp_GetUserIPD();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetUserIPD(float value);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern float ovrp_GetUserEyeDepth();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetUserEyeDepth(float value);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern float ovrp_GetUserEyeHeight();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetUserEyeHeight(float value);
	}

	private static class OVRP_1_2_0
	{
		public static readonly System.Version version = new System.Version(1, 2, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetSystemVSyncCount(int vsyncCount);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrpi_SetTrackingCalibratedOrigin();
	}

	private static class OVRP_1_3_0
	{
		public static readonly System.Version version = new System.Version(1, 3, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetEyeOcclusionMeshEnabled();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetEyeOcclusionMeshEnabled(Bool value);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetSystemHeadphonesPresent();
	}

	private static class OVRP_1_5_0
	{
		public static readonly System.Version version = new System.Version(1, 5, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern SystemRegion ovrp_GetSystemRegion();
	}

	private static class OVRP_1_6_0
	{
		public static readonly System.Version version = new System.Version(1, 6, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetTrackingIPDEnabled();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetTrackingIPDEnabled(Bool value);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern HapticsDesc ovrp_GetControllerHapticsDesc(uint controllerMask);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern HapticsState ovrp_GetControllerHapticsState(uint controllerMask);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetControllerHaptics(uint controllerMask, HapticsBuffer hapticsBuffer);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetOverlayQuad3(uint flags, IntPtr textureLeft, IntPtr textureRight, IntPtr device, Posef pose, Vector3f scale, int layerIndex);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern float ovrp_GetEyeRecommendedResolutionScale();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern float ovrp_GetAppCpuStartToGpuEndTime();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern int ovrp_GetSystemRecommendedMSAALevel();
	}

	private static class OVRP_1_7_0
	{
		public static readonly System.Version version = new System.Version(1, 7, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetAppChromaticCorrection();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetAppChromaticCorrection(Bool value);
	}

	private static class OVRP_1_8_0
	{
		public static readonly System.Version version = new System.Version(1, 8, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetBoundaryConfigured();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern BoundaryTestResult ovrp_TestBoundaryNode(Node nodeId, BoundaryType boundaryType);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern BoundaryTestResult ovrp_TestBoundaryPoint(Vector3f point, BoundaryType boundaryType);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetBoundaryLookAndFeel(BoundaryLookAndFeel lookAndFeel);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_ResetBoundaryLookAndFeel();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern BoundaryGeometry ovrp_GetBoundaryGeometry(BoundaryType boundaryType);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Vector3f ovrp_GetBoundaryDimensions(BoundaryType boundaryType);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetBoundaryVisible();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetBoundaryVisible(Bool value);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_Update2(int stateId, int frameIndex, double predictionSeconds);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Posef ovrp_GetNodePose2(int stateId, Node nodeId);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Posef ovrp_GetNodeVelocity2(int stateId, Node nodeId);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Posef ovrp_GetNodeAcceleration2(int stateId, Node nodeId);
	}

	private static class OVRP_1_9_0
	{
		public static readonly System.Version version = new System.Version(1, 9, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern SystemHeadset ovrp_GetSystemHeadsetType();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Controller ovrp_GetActiveController();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Controller ovrp_GetConnectedControllers();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetBoundaryGeometry2(BoundaryType boundaryType, IntPtr points, ref int pointsCount);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern AppPerfStats ovrp_GetAppPerfStats();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_ResetAppPerfStats();
	}

	private static class OVRP_1_10_0
	{
		public static readonly System.Version version = new System.Version(1, 10, 0);
	}

	private static class OVRP_1_11_0
	{
		public static readonly System.Version version = new System.Version(1, 11, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_SetDesiredEyeTextureFormat(EyeTextureFormat value);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern EyeTextureFormat ovrp_GetDesiredEyeTextureFormat();
	}

	private static class OVRP_1_12_0
	{
		public static readonly System.Version version = new System.Version(1, 12, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern float ovrp_GetAppFramerate();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern PoseStatef ovrp_GetNodePoseState(Step stepId, Node nodeId);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern ControllerState2 ovrp_GetControllerState2(uint controllerMask);
	}

	private static class OVRP_1_15_0
	{
		public const int OVRP_EXTERNAL_CAMERA_NAME_SIZE = 32;

		public static readonly System.Version version = new System.Version(1, 15, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_InitializeMixedReality();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_ShutdownMixedReality();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_GetMixedRealityInitialized();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_UpdateExternalCamera();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_GetExternalCameraCount(out int cameraCount);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_GetExternalCameraName(int cameraId, [MarshalAs(UnmanagedType.LPArray, SizeConst = OVRP_EXTERNAL_CAMERA_NAME_SIZE)] char[] cameraName);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_GetExternalCameraIntrinsics(int cameraId, out CameraIntrinsics cameraIntrinsics);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_GetExternalCameraExtrinsics(int cameraId, out CameraExtrinsics cameraExtrinsics);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_CalculateLayerDesc(OverlayShape shape, LayerLayout layout, ref Sizei textureSize,
			int mipLevels, int sampleCount, EyeTextureFormat format, int layerFlags, ref LayerDesc layerDesc);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_EnqueueSetupLayer(ref LayerDesc desc, IntPtr layerId);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_EnqueueDestroyLayer(IntPtr layerId);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_GetLayerTextureStageCount(int layerId, ref int layerTextureStageCount);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_GetLayerTexturePtr(int layerId, int stage, Eye eyeId, ref IntPtr textureHandle);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_EnqueueSubmitLayer(uint flags, IntPtr textureLeft, IntPtr textureRight, int layerId, int frameIndex, Posef pose, Vector3f scale, int layerIndex);
	}

	private static class OVRP_1_16_0
	{
		public static readonly System.Version version = new System.Version(1, 16, 0);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_UpdateCameraDevices();

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_IsCameraDeviceAvailable(CameraDevice cameraDevice);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_SetCameraDevicePreferredColorFrameSize(CameraDevice cameraDevice, Sizei preferredColorFrameSize);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_OpenCameraDevice(CameraDevice cameraDevice);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_CloseCameraDevice(CameraDevice cameraDevice);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_HasCameraDeviceOpened(CameraDevice cameraDevice);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Bool ovrp_IsCameraDeviceColorFrameAvailable(CameraDevice cameraDevice);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_GetCameraDeviceColorFrameSize(CameraDevice cameraDevice, out Sizei colorFrameSize);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_GetCameraDeviceColorFrameBgraPixels(CameraDevice cameraDevice, out IntPtr colorFrameBgraPixels, out int colorFrameRowPitch);

		[DllImport(pluginName, CallingConvention = CallingConvention.Cdecl)]
		public static extern Result ovrp_GetControllerState4(uint controllerMask, ref ControllerState4 controllerState);
	}
}
