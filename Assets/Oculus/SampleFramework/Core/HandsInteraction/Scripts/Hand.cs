/************************************************************************************

Copyright (c) Facebook Technologies, LLC and its affiliates. All rights reserved.

See SampleFramework license.txt for license terms.  Unless required by applicable law
or agreed to in writing, the sample code is provided “AS IS” WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied.  See the license for specific
language governing permissions and limitations under the license.

************************************************************************************/

using UnityEngine;

namespace OculusSampleFramework
{
	public class Hand : MonoBehaviour
	{
		// Skeleton Component
		public HandSkeleton Skeleton { get; private set; }
		// Physics  bone capsule collider component
		public HandPhysics Physics { get; private set; }
		// Skeleton visualizer component
		public SkeletonVisualizer SkeletonVis { get; private set; }
		// Mesh componenet
		public HandMesh HandMesh { get; private set; }
		public OVRPlugin.Hand HandType;
		// Latest hand state received 
		private OVRPlugin.HandState _currentState;
		private bool _isTracked = false;
		private PointerState _pointer;
		private HandConfidenceFader _confidenceFader;
		private Hand.HandTrackingConfidence _handConfidence = Hand.HandTrackingConfidence.None;
		private float _scaledAlpha = 0.0f;

		public enum HandTrackingConfidence
		{
			Low, High, None
		}

		// If Skeleton, Physics and HandMesh are not mandatory to be available.
		// If they are available, for hand to fully initialize, they would need to be initialized successfully aswell.
		public bool IsInitialized
		{
			get
			{
				return (!Skeleton || Skeleton.IsInitialized) &&
				  (!Physics || Physics.IsInitialized) && (!HandMesh || HandMesh.IsInitialized);
			}
		}

		public bool IsTracked
		{
			get
			{
				return _isTracked;
			}
			private set
			{
				_isTracked = value;
			}
		}

		public SkinnedMeshRenderer HandSkinedMeshRenderer
		{
			get
			{
				if (HandMesh)
				{
					return HandMesh.HandSkinedMeshRenderer;
				}
				else
				{
					return null;
				}
			}
		}

		public Mesh Mesh
		{
			get
			{
				if (HandMesh)
				{
					return HandMesh.Mesh;
				}
				else
				{
					return null;
				}
			}
		}

		// Calculated Alpha vlaue of the hand according to OVRPlugin.TrackingConfidence
		public float ScaledAlpha
		{
			get
			{
				return _scaledAlpha;
			}
			set
			{
				_scaledAlpha = value;
			}
		}

		public Hand.HandTrackingConfidence HandConfidence
		{
			get
			{
				return _handConfidence;
			}
			private set
			{
				_handConfidence = value;
			}
		}

		// Pointer position, rotation and status for far field pinch interactions.
		public PointerState Pointer
		{
			get
			{
				return _pointer;
			}
		}

		public struct PointerState
		{
			public Vector3 PointerPosition;
			public Quaternion PointerOrientation;
			public bool PointerStatusValid;
		}

		public OVRPlugin.HandState State
		{
			get
			{
				return _currentState;
			}
		}

		// Calculate hand alpha confidence according to number off occurencese of HIGH
		// and LOW confidence values.
		private class HandConfidenceFader
		{
			private static float MAX_ALPHA = 1.0F;
			private int _numberOfFrames;
			private float _minTrackedAlpha = 0.0f;
			private float _maxTrackedAlpha = MAX_ALPHA;
			private int _currentCount = 0;

			public HandConfidenceFader(int numberOfFramse)
			{
				_numberOfFrames = numberOfFramse;
			}

			public float NextAlphaValue(HandTrackingConfidence confidence)
			{
				var calculatedAlpha = 0.0f;
				switch (confidence)
				{
					case Hand.HandTrackingConfidence.High:
						_currentCount = Mathf.Min((_currentCount + 1), _numberOfFrames);
						calculatedAlpha = Mathf.Clamp(_currentCount / (float)_numberOfFrames, _minTrackedAlpha, _maxTrackedAlpha);
						break;
					case Hand.HandTrackingConfidence.Low:
						_currentCount = Mathf.Max((_currentCount - 1), 0);
						calculatedAlpha = Mathf.Clamp(_currentCount / (float)_numberOfFrames, _minTrackedAlpha, _maxTrackedAlpha);
						break;
					default:
						_currentCount = 0;
						calculatedAlpha = 0.0f;
						break;
				}
				return calculatedAlpha;
			}
		}

		private void Awake()
		{
			_currentState = new OVRPlugin.HandState();
			Skeleton = GetComponent<HandSkeleton>();
			Physics = GetComponent<HandPhysics>();
			SkeletonVis = GetComponent<SkeletonVisualizer>();
			HandMesh = GetComponent<HandMesh>();
			_confidenceFader = new HandConfidenceFader(40);
		}

		private void FixedUpdate()
		{
			if (IsInitialized)
			{
				UpdatePose(OVRPlugin.Step.Physics);
				if (Physics)
				{
					Physics.UpdatePose();
				}
			}
		}

		private void Update()
		{
			if (IsInitialized)
			{
				UpdatePose(OVRPlugin.Step.Render);
			}
		}

		private void UpdatePose(OVRPlugin.Step renderStep)
		{
			if (!OVRPlugin.GetHandState(renderStep, HandType, ref _currentState))
			{
				IsTracked = false;
				HandConfidence = Hand.HandTrackingConfidence.None;
				ScaledAlpha = _confidenceFader.NextAlphaValue(HandConfidence);
			}
			else
			{
				IsTracked = (_currentState.Status & OVRPlugin.HandStatus.HandTracked) ==
				  OVRPlugin.HandStatus.HandTracked;
				if (IsTracked)
				{
					HandConfidence = OVRPluginConfidenceToHand(_currentState.HandConfidence);
				}
				else
				{
					HandConfidence = Hand.HandTrackingConfidence.None;
				}
				// Fade hand according to confidence.
				ScaledAlpha = _confidenceFader.NextAlphaValue(HandConfidence);
				// Update Pointer
				_pointer.PointerPosition = _currentState.PointerPose.Position.FromFlippedZVector3f();
				_pointer.PointerOrientation = _currentState.PointerPose.Orientation.FromFlippedZQuatf();
				_pointer.PointerStatusValid = (_currentState.Status & OVRPlugin.HandStatus.InputStateValid) ==
				  OVRPlugin.HandStatus.InputStateValid;
			}
			if (HandMesh)
			{
				HandMesh.UpdatePose();
			}
			if (Skeleton)
			{
				Skeleton.UpdatePose(_currentState);
			}
		}

		public float PinchStrength(OVRPlugin.HandFinger finger)
		{
			if (IsTracked && _currentState.PinchStrength != null && 
				_currentState.PinchStrength.Length > (int)finger)
			{
				return _currentState.PinchStrength[(int)finger];
			}
			else
			{
				return 0.0f;
			}
		}

		public void ShowSkeleton(Hands.HandsVisualMode mode)
		{
			if (IsInitialized)
			{
				HandMesh.EnableMeshRenderer(mode);
				if (SkeletonVis)
				{
					SkeletonVis.EnableVisualization = mode > Hands.HandsVisualMode.Mesh ? true : false;
				}
			}
		}

		public static Hand.HandTrackingConfidence OVRPluginConfidenceToHand(OVRPlugin.TrackingConfidence confidence)
		{
			return confidence == OVRPlugin.TrackingConfidence.High ? Hand.HandTrackingConfidence.High : Hand.HandTrackingConfidence.Low;
		}
	}
}
