using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]
public class PBT_physicsOBJ : MonoBehaviour
{
    void Update()
    {
        if (Replayer.Replaying)
        {
            gameObject.EnsureNoneOf<ConstantForce>();
            Rigidbody rb = GetComponent<Rigidbody>();
			Destroy(rb);
            gameObject.EnsureNoneOf<Rigidbody>();
            gameObject.EnsureNoneOf<Collider>();
            gameObject.EnsureNoneOf<UnityStandardAssets.Utility.AutoMoveAndRotate>();
            gameObject.EnsureNoneOf<ChangeParent>();
            gameObject.EnsureNoneOf<WaitAndThen>();
            gameObject.EnsureNoneOf<RemoveComponent>();
        }
    }
}
