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
	public class HandMaterialModifier
	{
		private static int HAND_COLOR_ID = Shader.PropertyToID("_ColorBottom");
		private static Color SYSTEM_GESTURE_COLOR = new Color(1.0f, 1.0f, 0.57f, 1.0f);

		private Color _oldColor;
		// use boolean to avoid setting color continuously to material.
		private bool _setSystemGestureColor;

		public HandMaterialModifier(Material materialToAdjust)
		{
			_oldColor = materialToAdjust.GetColor(HAND_COLOR_ID);
		}

		public void UpdateHandMaterial(Hand hand, Material materialToAdjust)
		{
			bool systemGestureEngaged =
			  (hand.State.Status & OVRPlugin.HandStatus.SystemGestureInProgress) != 0;
			// if we set engaged system gesture but haven't set its color yet, do so
			if (systemGestureEngaged && !_setSystemGestureColor)
			{
				materialToAdjust.SetColor(HAND_COLOR_ID, SYSTEM_GESTURE_COLOR);
				_setSystemGestureColor = true;
			}
			else if (!systemGestureEngaged && _setSystemGestureColor)
			{
				materialToAdjust.SetColor(HAND_COLOR_ID, _oldColor);
				_setSystemGestureColor = false;
			}
		}
	}
}
