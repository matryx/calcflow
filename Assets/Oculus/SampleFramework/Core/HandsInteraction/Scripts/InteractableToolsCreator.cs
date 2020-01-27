/************************************************************************************

Copyright (c) Facebook Technologies, LLC and its affiliates. All rights reserved.  

See SampleFramework license.txt for license terms.  Unless required by applicable law 
or agreed to in writing, the sample code is provided “AS IS” WITHOUT WARRANTIES OR 
CONDITIONS OF ANY KIND, either express or implied.  See the license for specific 
language governing permissions and limitations under the license.

************************************************************************************/

using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace OculusSampleFramework
{
	/// <summary>
	/// Spawns all interactable tools that are specified for a scene.
	/// </summary>
	public class InteractableToolsCreator : MonoBehaviour
	{
		[SerializeField] private Transform[] LeftHandTools = null;
		[SerializeField] private Transform[] RightHandTools = null;

		private void Awake()
		{
			if (LeftHandTools != null && LeftHandTools.Length > 0)
			{
				StartCoroutine(AttachToolsToHands(LeftHandTools, false));
			}

			if (RightHandTools != null && RightHandTools.Length > 0)
			{
				StartCoroutine(AttachToolsToHands(RightHandTools, true));
			}
		}

		private IEnumerator AttachToolsToHands(Transform[] toolObjects, bool isRightHand)
		{
			Hands handsObj = null;
			while ((handsObj = Hands.Instance) == null || !handsObj.IsInitialized())
			{
				yield return null;
			}

			// create set of tools per hand to be safe
			HashSet<Transform> toolObjectSet = new HashSet<Transform>();
			foreach (Transform toolTransform in toolObjects)
			{
				toolObjectSet.Add(toolTransform.transform);
			}

			foreach (Transform toolObject in toolObjectSet)
			{
				Hand handToAttachTo =
				  isRightHand ? handsObj.RightHand : handsObj.LeftHand;
				while (handToAttachTo.Skeleton == null || handToAttachTo.Skeleton.Bones == null)
				{
					yield return null;
				}

				AttachToolToHandTransform(toolObject, isRightHand);
			}
		}

		private void AttachToolToHandTransform(Transform tool, bool isRightHanded)
		{
			var newTool = Instantiate(tool).transform;
			newTool.localPosition = Vector3.zero;
			var toolComp = newTool.GetComponent<InteractableTool>();
			toolComp.IsRightHandedTool = isRightHanded;
			// Initialize only AFTER settings have been applied!
			toolComp.Initialize();
		}
	}
}
