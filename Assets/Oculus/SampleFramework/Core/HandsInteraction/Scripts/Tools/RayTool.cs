/************************************************************************************

Copyright (c) Facebook Technologies, LLC and its affiliates. All rights reserved.  

See SampleFramework license.txt for license terms.  Unless required by applicable law 
or agreed to in writing, the sample code is provided “AS IS” WITHOUT WARRANTIES OR 
CONDITIONS OF ANY KIND, either express or implied.  See the license for specific 
language governing permissions and limitations under the license.

************************************************************************************/

using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Assertions;

/// <summary>
/// Ray tool used for far-field interactions.
/// </summary>
namespace OculusSampleFramework
{
	public class RayTool : MonoBehaviour, InteractableTool
	{
		private const float MINIMUM_RAY_CAST_DISTANCE = 0.8f;
		private const float COLLIDER_RADIUS = 0.01f;
		private const int NUM_MAX_PRIMARY_HITS = 10;
		private const int NUM_MAX_SECONDARY_HITS = 25;
		private const int NUM_COLLIDERS_TO_TEST = 20;

		[SerializeField] private RayToolView _rayToolView = null;
		[Range(0.0f, 45.0f)] [SerializeField] private float _coneAngleDegrees = 20.0f;
		[SerializeField] private float _farFieldMaxDistance = 5f;

		public Transform ToolTransform
		{
			get { return this.transform; }
		}
		public bool IsRightHandedTool { get; set; }
		public Dictionary<Interactable, InteractableCollisionInfo> CurrInteractableToCollisionInfos { get; set; }
		public Dictionary<Interactable, InteractableCollisionInfo> PrevInteractableToCollisionInfos { get; set; }

		public InteractableToolTags ToolTags
		{
			get
			{
				return InteractableToolTags.Ray;
			}
		}

		private PinchStateModule _pinchStateModule = new PinchStateModule();
		private Interactable _focusedInteractable;

		public ToolInputState ToolInputState
		{
			get
			{
				if (_pinchStateModule.PinchDownOnFocusedObject)
				{
					return ToolInputState.PrimaryInputDown;
				}
				if (_pinchStateModule.PinchSteadyOnFocusedObject)
				{
					return ToolInputState.PrimaryInputDownStay;
				}
				if (_pinchStateModule.PinchUpAndDownOnFocusedObject)
				{
					return ToolInputState.PrimaryInputUp;
				}

				return ToolInputState.Inactive;
			}
		}

		public bool IsFarFieldTool
		{
			get { return true; }
		}

		public Vector3 Velocity { get; private set; }
		public Vector3 InteractionPosition { get; private set; }

		public bool EnableState
		{
			get
			{
				return _rayToolView.EnableState;
			}
			set
			{
				_rayToolView.EnableState = value;
			}
		}

		private List<InteractableCollisionInfo> _intersectingObjects = new List<InteractableCollisionInfo>();
		private Collider[] _collidersOverlapped = new Collider[NUM_COLLIDERS_TO_TEST];

		private Interactable _currInteractableCastedAgainst = null;
		private float _coneAngleReleaseDegrees;

		private RaycastHit[] _primaryHits = new RaycastHit[NUM_MAX_PRIMARY_HITS];
		private Collider[] _secondaryOverlapResults = new Collider[NUM_MAX_SECONDARY_HITS];
		private bool _initialized = false;

		public void Initialize()
		{
			Assert.IsNotNull(_rayToolView);
			CurrInteractableToCollisionInfos = new Dictionary<Interactable, InteractableCollisionInfo>();
			PrevInteractableToCollisionInfos = new Dictionary<Interactable, InteractableCollisionInfo>();
			InteractableToolsInputRouter.Instance.RegisterInteractableTool(this);
			_rayToolView.InteractableTool = this;
			_coneAngleReleaseDegrees = _coneAngleDegrees * 1.2f;
			_initialized = true;
		}

		private void OnDestroy()
		{
			if (InteractableToolsInputRouter.Instance != null)
			{
				InteractableToolsInputRouter.Instance.UnregisterInteractableTool(this);
			}
		}

		private void Update()
		{
			if (!Hands.Instance.IsInitialized() || !_initialized)
			{
				return;
			}

			var hand = IsRightHandedTool ? Hands.Instance.RightHand : Hands.Instance.LeftHand;
			var pointer = hand.Pointer;
			transform.position = pointer.PointerPosition;
			transform.rotation = pointer.PointerOrientation;

			var prevPosition = InteractionPosition;
			var currPosition = transform.position;
			Velocity = (currPosition - prevPosition) / Time.deltaTime;
			InteractionPosition = currPosition;

			_pinchStateModule.UpdateState(hand, _focusedInteractable);
			_rayToolView.ToolActivateState = _pinchStateModule.PinchSteadyOnFocusedObject ||
			  _pinchStateModule.PinchDownOnFocusedObject;
		}

		/// <summary>
		/// Avoid hand collider during raycasts so move origin some distance away from where tool is.
		/// </summary>
		/// <returns>Proper raycast origin.</returns>
		private Vector3 GetRayCastOrigin()
		{
			return transform.position + MINIMUM_RAY_CAST_DISTANCE * transform.forward;
		}

		public List<InteractableCollisionInfo> GetIntersectingObjects()
		{
			if (!_initialized)
			{
				return _intersectingObjects;
			}

			// if we already have focused on something, keep it until the angle between
			// our forward direction and object vector becomes too large
			if (_currInteractableCastedAgainst != null &&
				HasRayReleasedInteractable(_currInteractableCastedAgainst))
			{
				// reset state
				_currInteractableCastedAgainst = null;
			}

			// Find target interactable if we haven't found one before.
			if (_currInteractableCastedAgainst == null)
			{
				_intersectingObjects.Clear();
				_currInteractableCastedAgainst = FindTargetInteractable();

				// If we have found one, query collision zones.
				if (_currInteractableCastedAgainst != null)
				{
					var targetHitPoint = _currInteractableCastedAgainst.transform.position;
					int numHits = Physics.OverlapSphereNonAlloc(targetHitPoint, COLLIDER_RADIUS, _collidersOverlapped);

					// find all colliders encountered; focus only on ones belonging to target element
					for (int i = 0; i < numHits; i++)
					{
						Collider colliderHit = _collidersOverlapped[i];
						var colliderZone = colliderHit.GetComponent<ColliderZone>();
						if (colliderZone == null)
						{
							continue;
						}

						Interactable interactableComponent = colliderZone.ParentInteractable;
						if (interactableComponent == null || interactableComponent
							!= _currInteractableCastedAgainst)
						{
							continue;
						}

						InteractableCollisionInfo collisionInfo = new InteractableCollisionInfo(colliderZone,
						  colliderZone.CollisionDepth, this);
						_intersectingObjects.Add(collisionInfo);
					}

					// clear intersecting object if no collisions were found
					if (_intersectingObjects.Count == 0)
					{
						_currInteractableCastedAgainst = null;
					}
				}
			}

			return _intersectingObjects;
		}

		private bool HasRayReleasedInteractable(Interactable focusedInteractable)
		{
			var ourPosition = transform.position;
			var forwardDirection = transform.forward;
			var hysteresisDotThreshold = Mathf.Cos(_coneAngleReleaseDegrees * Mathf.Deg2Rad);
			var vectorToFocusedObject = focusedInteractable.transform.position - ourPosition;
			vectorToFocusedObject.Normalize();
			var hysteresisDotProduct = Vector3.Dot(vectorToFocusedObject, forwardDirection);
			return hysteresisDotProduct < hysteresisDotThreshold;
		}

		/// <summary>
		/// Find all objects from primary ray cast or if that fails, all objects in a
		/// cone around main ray direction via a "secondary" cast.
		/// </summary>
		private Interactable FindTargetInteractable()
		{
			var rayOrigin = GetRayCastOrigin();
			var rayDirection = transform.forward;
			Interactable targetInteractable = null;

			// attempt primary ray cast
			targetInteractable = FindPrimaryRaycastHit(rayOrigin, rayDirection);

			// if primary cast fails, try secondary cone test
			if (targetInteractable == null)
			{
				targetInteractable = FindInteractableViaConeTest(rayOrigin, rayDirection);
			}

			return targetInteractable;
		}

		/// <summary>
		/// Find first hit that is supports our tool's method of interaction.
		/// </summary>
		private Interactable FindPrimaryRaycastHit(Vector3 rayOrigin, Vector3 rayDirection)
		{
			Interactable interactableCastedAgainst = null;

			// hit order not guaranteed, so find closest
			int numHits = Physics.RaycastNonAlloc(new Ray(rayOrigin, rayDirection), _primaryHits, Mathf.Infinity);
			float minDistance = 0.0f;
			for (int hitIndex = 0; hitIndex < numHits; hitIndex++)
			{
				RaycastHit raycastHit = _primaryHits[hitIndex];

				// continue if something occludes it and that object is not an interactable
				var currentHitColliderZone = raycastHit.transform.GetComponent<ColliderZone>();
				if (currentHitColliderZone == null)
				{
					continue;
				}

				// at this point we have encountered an interactable. Only consider it if
				// it allows interaction with our tool. Otherwise ignore it.
				Interactable currentInteractable = currentHitColliderZone.ParentInteractable;
				if (currentInteractable == null || (currentInteractable.ValidToolTagsMask & (int)ToolTags) == 0)
				{
					continue;
				}

				var vectorToInteractable = currentInteractable.transform.position - rayOrigin;
				var distanceToInteractable = vectorToInteractable.magnitude;
				if (interactableCastedAgainst == null || distanceToInteractable < minDistance)
				{
					interactableCastedAgainst = currentInteractable;
					minDistance = distanceToInteractable;
				}
			}

			return interactableCastedAgainst;
		}

		/// <summary>
		/// If primary cast fails, try secondary test to see if we can target an interactable.
		/// This target has to be far enough and support our tool via appropriate
		/// tags, and must be within a certain angle from our primary ray direction.
		/// </summary>
		/// <param name="rayOrigin">Primary ray origin.</param>
		/// <param name="rayDirection">Primary ray direction.</param>
		/// <returns>Interactable found, if any.</returns>
		private Interactable FindInteractableViaConeTest(Vector3 rayOrigin, Vector3 rayDirection)
		{
			Interactable targetInteractable = null;

			float minDistance = 0.0f;
			float minDotProductThreshold = Mathf.Cos(_coneAngleDegrees * Mathf.Deg2Rad);
			// cone extends from center line, where angle is split between top and bottom half
			float halfAngle = Mathf.Deg2Rad * _coneAngleDegrees * 0.5f;
			float coneRadius = Mathf.Tan(halfAngle) * _farFieldMaxDistance;

			int numColliders = Physics.OverlapBoxNonAlloc(
			  rayOrigin + rayDirection * _farFieldMaxDistance * 0.5f, // center
			  new Vector3(coneRadius, coneRadius, _farFieldMaxDistance * 0.5f), //half extents
			  _secondaryOverlapResults, transform.rotation);

			for (int i = 0; i < numColliders; i++)
			{
				Collider colliderHit = _secondaryOverlapResults[i];
				// skip invalid colliders
				var colliderZone = colliderHit.GetComponent<ColliderZone>();
				if (colliderZone == null)
				{
					continue;
				}

				// skip invalid components
				Interactable interactableComponent = colliderZone.ParentInteractable;
				if (interactableComponent == null ||
					(interactableComponent.ValidToolTagsMask & (int)ToolTags) == 0)
				{
					continue;
				}

				var vectorToInteractable = interactableComponent.transform.position - rayOrigin;
				var distanceToInteractable = vectorToInteractable.magnitude;
				vectorToInteractable /= distanceToInteractable;
				var dotProduct = Vector3.Dot(vectorToInteractable, rayDirection);
				// must be inside cone!
				if (dotProduct < minDotProductThreshold)
				{
					continue;
				}

				if (targetInteractable == null || distanceToInteractable < minDistance)
				{
					targetInteractable = interactableComponent;
					minDistance = distanceToInteractable;
				}
			}

			return targetInteractable;
		}

		public void FocusOnInteractable(Interactable focusedInteractable,
		  ColliderZone colliderZone)
		{
			_rayToolView.SetFocusedInteractable(focusedInteractable);
			_focusedInteractable = focusedInteractable;
		}

		public void DeFocus()
		{
			_rayToolView.SetFocusedInteractable(null);
			_focusedInteractable = null;
		}
	}
}
