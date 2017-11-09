using UnityEngine;
using System.Collections;
//using OvrTouch.Hands;
using NanoVRController;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]

public class LeashedGrabbable : GrabbableObject {

    protected FixedJoint joint;

    Vector3 originalPos;

    protected override void Start()
    {
        base.Start();
        originalPos = transform.position;
    }

    protected override void Update()
    {
        base.Update();
        if (transform.parent == null)
        {
            if (GetComponent<Rigidbody>().velocity.magnitude > 0 && (originalPos - transform.position).magnitude >= 5f)
            {
                //GetComponent<Rigidbody>().velocity = Vector3.Lerp(GetComponent<Rigidbody>().velocity, Vector3.zero, 0.1f);
                //GetComponent<Rigidbody>().angularVelocity = Vector3.Lerp(GetComponent<Rigidbody>().angularVelocity, Vector3.zero, 0.1f);
                GetComponent<Rigidbody>().AddForce((originalPos - transform.position) * (originalPos-transform.position).magnitude, ForceMode.Force);
            }
            else if((originalPos - transform.position).magnitude < 5f)
            {
                GetComponent<Rigidbody>().velocity = Vector3.zero;
                GetComponent<Rigidbody>().angularVelocity = Vector3.zero;
                transform.position = Vector3.Lerp(transform.position, originalPos, 0.1f);
            }
        }
    }


    void OnCollisionEnter(Collision c)
    {
        if (!isGrabbed)
        {
            GetComponent<Rigidbody>().AddForce((transform.position - c.contacts[0].point), ForceMode.Impulse);
        }
    }
}
