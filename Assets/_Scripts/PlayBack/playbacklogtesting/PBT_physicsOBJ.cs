using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;
public class PBT_physicsOBJ : MonoBehaviour {

	void Update () {
		if (Replayer.Replaying){
			gameObject.EnsureNoneOf<ConstantForce>();
			gameObject.EnsureNoneOf<Rigidbody>();
			gameObject.EnsureNoneOf<Collider>();
			gameObject.EnsureNoneOf<UnityStandardAssets.Utility.AutoMoveAndRotate>();
		}
	}
}
