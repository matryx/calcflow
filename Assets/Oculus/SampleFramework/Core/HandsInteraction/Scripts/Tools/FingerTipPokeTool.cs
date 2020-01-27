/************************************************************************************

Copyright (c) Facebook Technologies, LLC and its affiliates. All rights reserved.  

See SampleFramework license.txt for license terms.  Unless required by applicable law 
or agreed to in writing, the sample code is provided “AS IS” WITHOUT WARRANTIES OR 
CONDITIONS OF ANY KIND, either express or implied.  See the license for specific 
language governing permissions and limitations under the license.

************************************************************************************/

using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Assertions;

namespace OculusSampleFramework
{
	/// <summary>
	/// Poke tool used for near-field (touching) interactions. Assumes that it will be placed on
	/// finger tips.
	/// </summary>
	public class FingerTipPokeTool : MonoBehaviour, InteractableTool
	{
		private const int NUM_VELOCITY_FRAMES = 10;

		[SerializeField] private FingerTipPokeToolView _fingerTipPokeToolView = null;
		[SerializeField] private OVRPlugin.HandFinger _fingerToFollow = OVRPlugin.HandFinger.Index;

		public bool IsRightHandedTool { get; set; }
		public Dictionary<Interactable, InteractableCollisionInfo> CurrInteractableToCollisionInfos
		{ get; set; }
		public Dictionary<Interactable, InteractableCollisionInfo> PrevInteractableToCollisionInfos
		{ get; set; }
		public Vector3 Velocity { get; private set; }
		public Vector3 InteractionPosition { get; private set; }

		public InteractableToolTags ToolTags
		{
			get
			{
				return InteractableToolTags.Poke;
			}
		}
		public ToolInputState ToolInputState
		{
			get
			{
				return ToolInputState.Inactive;
			}
		}
		public bool IsFarFieldTool
		{
			get
			{
				return false;
			}
		}
		public Transform ToolTransform
		{
			get
			{
				return this.transform;
			}
		}
		public bool EnableState
		{
			get
			{
				return _fingerTipPokeToolView.gameObject.activeSelf;
			}
			set
			{
				_fingerTipPokeToolView.gameObject.SetActive(value);
			}
		}

		private Vector3[] _velocityFrames;
		private int _currVelocityFrame = 0;
		private bool _sampledMaxFramesAlready;
		private Vector3 _position;
		private List<InteractableCollisionInfo> _intersectingObjects = new List<InteractableCollisionInfo>();

		private BoneCapsuleTriggerLogic[] _boneCapsuleTriggerLogic;

		private float _lastScale = 1.0f;
		private bool _isInitialized = false;
		private CapsuleInfo _capsuleToTrack;

		public void Initialize()
		{
			Assert.IsNotNull(_fingerTipPokeToolView);

			CurrInteractableToCollisionInfos = new Dictionary<Interactable, InteractableCollisionInfo>();
			PrevInteractableToCollisionInfos = new Dictionary<Interactable, InteractableCollisionInfo>();

			InteractableToolsInputRouter.Instance.RegisterInteractableTool(this);
			_fingerTipPokeToolView.InteractableTool = this;

			_velocityFrames = new Vector3[NUM_VELOCITY_FRAMES];
			Array.Clear(_velocityFrames, 0, NUM_VELOCITY_FRAMES);

			StartCoroutine(AttachTriggerLogic());
		}

		private IEnumerator AttachTriggerLogic()
		{
			while (!Hands.Instance.IsInitialized())
			{
				yield return null;
			}

			Hand properHand = IsRightHandedTool ? Hands.Instance.RightHand : Hands.Instance.LeftHand;
			HandPhysics handPhysics = properHand.Physics;

			while (!handPhysics.IsInitialized)
			{
				yield return null;
			}

			OVRPlugin.BoneId boneToTestCollisions = OVRPlugin.BoneId.Hand_Pinky3;
			switch (_fingerToFollow)
			{
				case OVRPlugin.HandFinger.Thumb:
					boneToTestCollisions = OVRPlugin.BoneId.Hand_Thumb3;
					break;
				case OVRPlugin.HandFinger.Index:
					boneToTestCollisions = OVRPlugin.BoneId.Hand_Index3;
					break;
				case OVRPlugin.HandFinger.Middle:
					boneToTestCollisions = OVRPlugin.BoneId.Hand_Middle3;
					break;
				case OVRPlugin.HandFinger.Ring:
					boneToTestCollisions = OVRPlugin.BoneId.Hand_Ring3;
					break;
				default:
					boneToTestCollisions = OVRPlugin.BoneId.Hand_Pinky3;
					break;
			}

			List<BoneCapsuleTriggerLogic> boneCapsuleTriggerLogic = new List<BoneCapsuleTriggerLogic>();
			List<CapsuleInfo> boneCapsules = handPhysics.GetCapsulesPerBone(boneToTestCollisions);
			foreach (var ovrCapsuleInfo in boneCapsules)
			{
				var boneCapsuleTrigger = ovrCapsuleInfo.gameObject.AddComponent<BoneCapsuleTriggerLogic>();
				ovrCapsuleInfo.CapsuleCollider.isTrigger = true;
				boneCapsuleTrigger.ToolTags = ToolTags;
				boneCapsuleTriggerLogic.Add(boneCapsuleTrigger);
			}

			_boneCapsuleTriggerLogic = boneCapsuleTriggerLogic.ToArray();
			// finger tip should have only one capsule
			if (boneCapsules.Count > 0)
			{
				_capsuleToTrack = boneCapsules[0];
			}

			_isInitialized = true;
		}

		private void Update()
		{
			if (!Hands.Instance.IsInitialized() || !_isInitialized || _capsuleToTrack == null)
			{
				return;
			}

			Hand hand = IsRightHandedTool ? Hands.Instance.RightHand : Hands.Instance.LeftHand;
			float currentScale = hand.State.HandScale;
			// push tool into the tip based on how wide it is. so negate the direction
			Transform capsuleTransform = _capsuleToTrack.transform;
			Vector3 capsuleDirection = (IsRightHandedTool ? -capsuleTransform.right : capsuleTransform.right);
			Vector3 trackedPosition = capsuleTransform.position + _capsuleToTrack.CapsuleCollider.height * 0.5f
			  * capsuleDirection;
			Vector3 sphereRadiusOffset = currentScale * _fingerTipPokeToolView.SphereRadius *
			  capsuleDirection;
			// push tool back so that it's centered on transform/bone
			Vector3 toolPosition = trackedPosition + sphereRadiusOffset;
			transform.position = toolPosition;
			transform.rotation = capsuleTransform.rotation;
			InteractionPosition = trackedPosition;

			UpdateAverageVelocity();

			if (!Hands.Instance.IsInitialized())
			{
				return;
			}

			CheckAndUpdateScale();
		}

		private void UpdateAverageVelocity()
		{
			var prevPosition = _position;
			var currPosition = transform.position;
			var currentVelocity = (currPosition - prevPosition) / Time.deltaTime;
			_position = currPosition;
			_velocityFrames[_currVelocityFrame] = currentVelocity;
			// if sampled more than allowed, loop back toward the beginning
			_currVelocityFrame = (_currVelocityFrame + 1) % NUM_VELOCITY_FRAMES;

			Velocity = Vector3.zero;
			// edge case; when we first start up, we will have only sampled less than the
			// max frames. so only compute the average over that subset. After that, the
			// frame samples will act like an array that loops back toward to the beginning
			if (!_sampledMaxFramesAlready && _currVelocityFrame == NUM_VELOCITY_FRAMES - 1)
			{
				_sampledMaxFramesAlready = true;
			}

			int numFramesToSamples = _sampledMaxFramesAlready ? NUM_VELOCITY_FRAMES : _currVelocityFrame + 1;
			for (int frameIndex = 0; frameIndex < numFramesToSamples; frameIndex++)
			{
				Velocity += _velocityFrames[frameIndex];
			}

			Velocity /= numFramesToSamples;
		}

		private void CheckAndUpdateScale()
		{
			float currentScale = IsRightHandedTool ? Hands.Instance.RightHand.State.HandScale
			  : Hands.Instance.LeftHand.State.HandScale;
			if (Mathf.Abs(currentScale - _lastScale) > Mathf.Epsilon)
			{
				transform.localScale = new Vector3(currentScale, currentScale, currentScale);
				_lastScale = currentScale;
			}
		}

		public List<InteractableCollisionInfo> GetIntersectingObjects()
		{
			_intersectingObjects.Clear();

			foreach (var boneCapsuleTriggerLogic in _boneCapsuleTriggerLogic)
			{
				var collidersTouching = boneCapsuleTriggerLogic.CollidersTouchingUs;
				foreach (ColliderZone colliderTouching in collidersTouching)
				{
					_intersectingObjects.Add(new InteractableCollisionInfo(colliderTouching,
					  colliderTouching.CollisionDepth, this));
				}
			}

			return _intersectingObjects;
		}

		public void FocusOnInteractable(Interactable focusedInteractable,
		  ColliderZone colliderZone)
		{
			// no need for focus
		}

		public void DeFocus()
		{
			// no need for focus
		}
	}
}
