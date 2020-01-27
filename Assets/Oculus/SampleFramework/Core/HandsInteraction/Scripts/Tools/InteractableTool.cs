/************************************************************************************

Copyright (c) Facebook Technologies, LLC and its affiliates. All rights reserved.  

See SampleFramework license.txt for license terms.  Unless required by applicable law 
or agreed to in writing, the sample code is provided “AS IS” WITHOUT WARRANTIES OR 
CONDITIONS OF ANY KIND, either express or implied.  See the license for specific 
language governing permissions and limitations under the license.

************************************************************************************/

using System.Collections.Generic;
using UnityEngine;

namespace OculusSampleFramework
{
	/// <summary>
	/// Describes how the tool will work with interactables. An interactable,
	/// in turn, can tell us which tools they support via their flag bit mask.
	/// </summary>
	[System.Flags]
	public enum InteractableToolTags
	{
		None = 0,
		Ray = 1 << 0,
		Poke = 1 << 2,
		Pinch = 1 << 3,
		All = ~0
	}

	/// <summary>
	/// Indicates if tool has been activated via some gesture, press, etc.
	/// </summary>
	public enum ToolInputState
	{
		Inactive = 0,
		PrimaryInputDown,
		PrimaryInputDownStay,
		PrimaryInputUp
	}

	/// <summary>
	/// Describes tool-to-collision information.
	/// </summary>
	public class InteractableCollisionInfo
	{
		public InteractableCollisionInfo(ColliderZone collider, InteractableCollisionDepth collisionDepth,
		  InteractableTool collidingTool)
		{
			InteractableCollider = collider;
			CollisionDepth = collisionDepth;
			CollidingTool = collidingTool;
		}

		public ColliderZone InteractableCollider;
		public InteractableCollisionDepth CollisionDepth;
		public InteractableTool CollidingTool;
	}

	/// <summary>
	/// A tool that can engage interactables.
	/// </summary>
	public interface InteractableTool
	{
		Transform ToolTransform { get; }
		bool IsRightHandedTool { get; set; }

		Dictionary<Interactable, InteractableCollisionInfo> CurrInteractableToCollisionInfos
		{ get; set; }
		Dictionary<Interactable, InteractableCollisionInfo> PrevInteractableToCollisionInfos
		{ get; set; }

		InteractableToolTags ToolTags { get; }

		ToolInputState ToolInputState { get; }

		bool IsFarFieldTool { get; }

		Vector3 Velocity { get; }

		/// <summary>
		/// Sometimes we want the position of a tool for stuff like pokes.
		/// </summary>
		Vector3 InteractionPosition { get; }

		/// <summary>
		/// Finds and returns objects that interact with this tool.
		/// Whether it is via cast, poking, pinching, etc.
		/// </summary>
		List<InteractableCollisionInfo> GetIntersectingObjects();

		/// <summary>
		/// Used to tell the tool to "focus" on a specific object, if
		/// focusing is indeed possible given the tool type.
		/// </summary>
		/// <param name="focusedInteractable">Interactable to focus.</param>
		/// <param name="colliderZone">Collider zone of interactable.</param>
		void FocusOnInteractable(Interactable focusedInteractable,
		  ColliderZone colliderZone);

		void Initialize();

		void DeFocus();

		bool EnableState { get; set; }
	}

}
