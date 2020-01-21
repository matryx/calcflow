/************************************************************************************

Copyright (c) Facebook Technologies, LLC and its affiliates. All rights reserved.

See SampleFramework license.txt for license terms.  Unless required by applicable law
or agreed to in writing, the sample code is provided “AS IS” WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied.  See the license for specific
language governing permissions and limitations under the license.

************************************************************************************/

using UnityEngine;
using UnityEngine.Assertions;

namespace OculusSampleFramework
{
	public class Hands : MonoBehaviour
	{
		// If _handPrefab is set in the editor - both _leftHand and 
		// _rightHand do not need to be set and will not be visible in editor.
		// See OVRHandsEditor.cs
		[SerializeField]
		private Hand _handPrefab = null;
		// Setting this field in editor will not instantiate a new left hand gameObject
		[SerializeField]
		private Hand _leftHand = null;
		// Setting this field in editor will not instantiate a new right hand gameObject
		[SerializeField]
		private Hand _rightHand = null;

		//Instance should only be called after Awake has been called
		public static Hands Instance { get; private set; }
		public HandsVisualMode VisualMode = HandsVisualMode.Mesh;

		public Hand LeftHand
		{
			get
			{
				return _leftHand;
			}
			private set
			{
				_leftHand = value;
			}
		}
		public Hand RightHand
		{
			get
			{
				return _rightHand;
			}
			private set
			{
				_rightHand = value;
			}
		}

		public enum HandsVisualMode
		{
			Mesh = 0, Skeleton = 1, Both = 2, Max = 3
		}

		public bool IsInitialized()
		{
			return LeftHand && LeftHand.IsInitialized && RightHand && RightHand.IsInitialized;
		}

		private void Awake()
		{
			if (Instance && Instance != this)
			{
				Destroy(this);
				return;
			}
			Instance = this;
			Assert.IsTrue(_handPrefab || (LeftHand && RightHand));
			InitHands();
		}

		private void Update()
		{
			if (IsInitialized())
			{
				LeftHand.ShowSkeleton(VisualMode);
				RightHand.ShowSkeleton(VisualMode);
			}
		}

		// If Hands are already set not need to initialize them
		private void InitHands()
		{
			if (!LeftHand)
			{
				LeftHand = Instantiate(_handPrefab, transform, false);
				LeftHand.name = OVRPlugin.Hand.HandLeft.ToString();
				LeftHand.HandType = OVRPlugin.Hand.HandLeft;
			}
			if (!RightHand)
			{
				RightHand = Instantiate(_handPrefab, transform, false);
				RightHand.name = OVRPlugin.Hand.HandRight.ToString();
				RightHand.HandType = OVRPlugin.Hand.HandRight;
			}
		}

		public void SwitchVisualization()
		{
			VisualMode = (HandsVisualMode)(((int)VisualMode + 1) % (int)HandsVisualMode.Max);
		}
	}
}


