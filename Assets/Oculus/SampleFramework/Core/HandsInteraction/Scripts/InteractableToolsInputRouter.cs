/************************************************************************************

Copyright (c) Facebook Technologies, LLC and its affiliates. All rights reserved.  

See SampleFramework license.txt for license terms.  Unless required by applicable law 
or agreed to in writing, the sample code is provided “AS IS” WITHOUT WARRANTIES OR 
CONDITIONS OF ANY KIND, either express or implied.  See the license for specific 
language governing permissions and limitations under the license.

************************************************************************************/

using System.Collections.Generic;
using System.Linq;
using UnityEngine;

namespace OculusSampleFramework
{
	/// <summary>
	/// Routes all collisions from interactable tools to the interactables themselves.
	/// We want to do this in a top-down fashion, because we might want to disable
	/// far-field interactions if near-field interactions take precendence (for instance).
	/// </summary>
	public class InteractableToolsInputRouter : MonoBehaviour
	{
		private static InteractableToolsInputRouter _instance;
		private bool _leftPinch, _rightPinch;

		public static InteractableToolsInputRouter Instance
		{
			get
			{
				if (_instance == null)
				{
					var instances = FindObjectsOfType<InteractableToolsInputRouter>();
					if (instances.Length > 0)
					{
						_instance = instances[0];
						// remove extras, if any
						for (int i = 1; i < instances.Length; i++)
						{
							GameObject.Destroy(instances[i].gameObject);
						}
					}
				}

				return _instance;
			}
		}

		private HashSet<InteractableTool> _leftHandNearTools = new HashSet<InteractableTool>();
		private HashSet<InteractableTool> _leftHandFarTools = new HashSet<InteractableTool>();
		private HashSet<InteractableTool> _rightHandNearTools = new HashSet<InteractableTool>();
		private HashSet<InteractableTool> _rightHandFarTools = new HashSet<InteractableTool>();

		// lists created once so that they don't need to be created per frame
		private List<Interactable> _addedInteractables = new List<Interactable>();
		private List<Interactable> _removedInteractables = new List<Interactable>();
		private List<Interactable> _remainingInteractables = new List<Interactable>();

		public void RegisterInteractableTool(InteractableTool interactableTool)
		{
			if (interactableTool.IsRightHandedTool)
			{
				if (interactableTool.IsFarFieldTool)
				{
					_rightHandFarTools.Add(interactableTool);
				}
				else
				{
					_rightHandNearTools.Add(interactableTool);
				}
			}
			else
			{
				if (interactableTool.IsFarFieldTool)
				{
					_leftHandFarTools.Add(interactableTool);
				}
				else
				{
					_leftHandNearTools.Add(interactableTool);
				}
			}
		}

		public void UnregisterInteractableTool(InteractableTool interactableTool)
		{
			if (interactableTool.IsRightHandedTool)
			{
				if (interactableTool.IsFarFieldTool)
				{
					_rightHandFarTools.Remove(interactableTool);
				}
				else
				{
					_rightHandNearTools.Remove(interactableTool);
				}
			}
			else
			{
				if (interactableTool.IsFarFieldTool)
				{
					_leftHandFarTools.Remove(interactableTool);
				}
				else
				{
					_leftHandNearTools.Remove(interactableTool);
				}
			}
		}

		private void Update()
		{
			if (!Hands.Instance.IsInitialized())
			{
				return;
			}

			bool leftHandIsReliable = Hands.Instance.LeftHand.HandConfidence == Hand.HandTrackingConfidence.High;
			bool rightHandIsReliable = Hands.Instance.RightHand.HandConfidence == Hand.HandTrackingConfidence.High;
			// make sure hand is not being used for system gesture, and pointer is valid
			bool leftHandProperlyTracked =
			  (Hands.Instance.LeftHand.State.Status & OVRPlugin.HandStatus.InputStateValid) != 0;
			bool rightHandProperlyTracked =
			  (Hands.Instance.RightHand.State.Status & OVRPlugin.HandStatus.InputStateValid) != 0;

			bool encounteredNearObjectsLeftHand = UpdateToolsAndEnableState(_leftHandNearTools, leftHandIsReliable);
			// don't interact with far field if near field is touching something
			UpdateToolsAndEnableState(_leftHandFarTools, !encounteredNearObjectsLeftHand && leftHandIsReliable &&
			  leftHandProperlyTracked);

			bool encounteredNearObjectsRightHand = UpdateToolsAndEnableState(_rightHandNearTools, rightHandIsReliable);
			// don't interact with far field if near field is touching something
			UpdateToolsAndEnableState(_rightHandFarTools, !encounteredNearObjectsRightHand && rightHandIsReliable &&
			  rightHandProperlyTracked);
		}

		private bool UpdateToolsAndEnableState(HashSet<InteractableTool> tools, bool toolsAreEnabledThisFrame)
		{
			bool encounteredObjects = UpdateTools(tools, resetCollisionData: !toolsAreEnabledThisFrame);
			ToggleToolsEnableState(tools, toolsAreEnabledThisFrame);
			return encounteredObjects;
		}

		/// <summary>
		/// Update tools specified based on new collisions.
		/// </summary>
		/// <param name="tools">Tools to update.</param>
		/// <param name="resetCollisionData">True if we want the tool to be disabled. This can happen
		/// if near field tools take precedence over far-field tools, for instance.</param>
		/// <returns></returns>
		private bool UpdateTools(HashSet<InteractableTool> tools, bool resetCollisionData = false)
		{
			bool toolsEncounteredObjects = false;

			foreach (InteractableTool currentInteractableTool in tools)
			{
				List<InteractableCollisionInfo> intersectingObjectsFound = currentInteractableTool.GetIntersectingObjects();

				if (intersectingObjectsFound.Count > 0 && !resetCollisionData)
				{
					if (!toolsEncounteredObjects)
					{
						toolsEncounteredObjects = intersectingObjectsFound.Count > 0;
					}

					// create map that indicates the furthest collider encountered per interactable element
					UpdateInteractableDeepestCollisionMap(intersectingObjectsFound,
					  currentInteractableTool.CurrInteractableToCollisionInfos);

					if (currentInteractableTool.IsFarFieldTool)
					{
						var firstInteractable = currentInteractableTool.CurrInteractableToCollisionInfos.First();
						// if our tool is activated, make sure depth is set to "action"
						if (currentInteractableTool.ToolInputState == ToolInputState.PrimaryInputUp)
						{
							firstInteractable.Value.InteractableCollider = firstInteractable.Key.ActionCollider;
							firstInteractable.Value.CollisionDepth = InteractableCollisionDepth.Action;
						}
						else
						{
							firstInteractable.Value.InteractableCollider = firstInteractable.Key.ContactCollider;
							firstInteractable.Value.CollisionDepth = InteractableCollisionDepth.Contact;
						}

						// far field tools only can focus elements -- pick first (for now)
						currentInteractableTool.FocusOnInteractable(firstInteractable.Key,
						  firstInteractable.Value.InteractableCollider);
					}
				}
				else
				{
					currentInteractableTool.DeFocus();
					currentInteractableTool.CurrInteractableToCollisionInfos.Clear();
				}

				UpdateUsingOldNewCollisionData(currentInteractableTool,
				  currentInteractableTool.PrevInteractableToCollisionInfos,
				  currentInteractableTool.CurrInteractableToCollisionInfos);
				currentInteractableTool.PrevInteractableToCollisionInfos =
				  new Dictionary<Interactable, InteractableCollisionInfo>(
					currentInteractableTool.CurrInteractableToCollisionInfos);
			}

			return toolsEncounteredObjects;
		}

		private void ToggleToolsEnableState(HashSet<InteractableTool> tools, bool enableState)
		{
			foreach (InteractableTool tool in tools)
			{
				if (tool.EnableState != enableState)
				{
					tool.EnableState = enableState;
				}
			}
		}

		/// <summary>
		/// For each interactable, update meta data to indicate deepest collision only.
		/// </summary>
		/// <param name="allCollisions">All current collisions.</param>
		/// <param name="interactableToCollisionInfo">Interactable->collision info map.</param>
		private void UpdateInteractableDeepestCollisionMap(List<InteractableCollisionInfo> allCollisions,
		  Dictionary<Interactable, InteractableCollisionInfo> interactableToCollisionInfo)
		{
			interactableToCollisionInfo.Clear();
			foreach (InteractableCollisionInfo interactableCollisionInfo in allCollisions)
			{
				var interactable = interactableCollisionInfo.InteractableCollider.ParentInteractable;
				var depth = interactableCollisionInfo.CollisionDepth;
				InteractableCollisionInfo collisionInfoFromMap = null;

				if (!interactableToCollisionInfo.TryGetValue(interactable, out collisionInfoFromMap))
				{
					interactableToCollisionInfo[interactable] = interactableCollisionInfo;
				}
				else if (collisionInfoFromMap.CollisionDepth < depth)
				{
					collisionInfoFromMap.InteractableCollider = interactableCollisionInfo.InteractableCollider;
					collisionInfoFromMap.CollisionDepth = depth;
				}
			}
		}

		/// <summary>
		/// If our collision information changed per frame, make note of it. Removed, added and remaining
		/// objects must get their proper events.
		/// </summary>
		/// <param name="oldCollisionMap">Previous collision information.</param>
		/// <param name="newCollisionMap">Current collision information.</param>
		private void UpdateUsingOldNewCollisionData(InteractableTool interactableTool,
		  Dictionary<Interactable, InteractableCollisionInfo> oldCollisionMap,
		  Dictionary<Interactable, InteractableCollisionInfo> newCollisionMap)
		{
			_addedInteractables.Clear();
			_removedInteractables.Clear();
			_remainingInteractables.Clear();

			foreach (Interactable key in newCollisionMap.Keys)
			{
				if (!oldCollisionMap.ContainsKey(key))
				{
					_addedInteractables.Add(key);
				}
				else
				{
					_remainingInteractables.Add(key);
				}
			}

			foreach (Interactable key in oldCollisionMap.Keys)
			{
				if (!newCollisionMap.ContainsKey(key))
				{
					_removedInteractables.Add(key);
				}
			}

			// tell removed interactables that we are gone
			foreach (Interactable removedInteractable in _removedInteractables)
			{
				removedInteractable.UpdateCollisionDepth(interactableTool, oldCollisionMap[removedInteractable].CollisionDepth,
				  InteractableCollisionDepth.None, oldCollisionMap[removedInteractable].CollidingTool);
			}

			// tell added interactable what state we are now in
			foreach (Interactable addedInteractableKey in _addedInteractables)
			{
				var addedInteractable = newCollisionMap[addedInteractableKey];
				var collisionDepth = addedInteractable.CollisionDepth;
				addedInteractableKey.UpdateCollisionDepth(interactableTool, InteractableCollisionDepth.None,
				  collisionDepth, newCollisionMap[addedInteractableKey].CollidingTool);
			}

			// remaining interactables must be updated
			foreach (Interactable remainingInteractableKey in _remainingInteractables)
			{
				var newDepth = newCollisionMap[remainingInteractableKey].CollisionDepth;
				var oldDepth = oldCollisionMap[remainingInteractableKey].CollisionDepth;
				remainingInteractableKey.UpdateCollisionDepth(interactableTool, oldDepth, newDepth,
				  newCollisionMap[remainingInteractableKey].CollidingTool);
			}
		}
	}
}
