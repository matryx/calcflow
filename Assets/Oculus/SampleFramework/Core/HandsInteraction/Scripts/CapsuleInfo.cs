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
	/// <summary>
	/// Easy access from capsule to hand, bone and rigidbody its associated with.
	/// </summary>
	public class CapsuleInfo : MonoBehaviour
	{
		public Hand Hand { get; private set; }
		public OVRPlugin.BoneId BoneIndex { get; private set; }
		public Rigidbody CapsuleRigidBBody { get; private set; }
		public CapsuleCollider CapsuleCollider { get; private set; }

		public void Init(Hand hand, OVRPlugin.BoneId bone,
		  Rigidbody rigidBody, CapsuleCollider capsuleCollider)
		{
			Hand = hand;
			BoneIndex = bone;
			CapsuleRigidBBody = rigidBody;
			CapsuleCollider = capsuleCollider;
		}
	}
}
