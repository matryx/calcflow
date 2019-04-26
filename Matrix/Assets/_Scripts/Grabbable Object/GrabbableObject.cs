using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System;
using NanoVRController;

public class GrabbableObject : Grippable
{
    public bool momentumOn = true;

    protected override void Update() {
        base.Update();
        if(activeGrabbers.Count>0)
            UpdatePivot();
    }

    protected override void OnRegisterController(Grabber g)
    {
        KillMomentum();

        AttachToNewPivot(false);
    }

    protected override void OnReleaseController(Grabber g)
    {
        if (activeGrabbers.Count > 0)
        {
            AttachToNewPivot(false);
        }
        else
        {
            DetachFromPivot();
            DeletePivot();
            if (momentumOn)
            {
                TransferMomentum(g.controller);
            }
        }
    }

    protected void AttachToNewPivot(bool keepParent)
    {
        DetachFromPivot();
        DeletePivot();
        CreatePivot();
        AttachToPivot(pivot.transform, keepParent);
    }

    protected void TransferMomentum(VRController c)
    {
        Rigidbody r = GetComponent<Rigidbody>();

        if (r != null)
        {
            r.velocity = c.Velocity;
            r.angularVelocity = c.AngularVelocity;
        }
    }

    protected void KillMomentum()
    {
        Rigidbody rb = GetComponent<Rigidbody>();
        if (rb != null)
        {
            rb.velocity = Vector3.zero;
            rb.angularVelocity = Vector3.zero;
        }
    }

    protected GameObject pivot;
    protected virtual void CreatePivot()
    {
        pivot = new GameObject();
        pivot.name = "pivot";
        UpdatePivot();
    }

    protected virtual void UpdatePivot()
    {
        if (activeGrabbers.Count == 1)
        {
            pivot.transform.position = activeGrabbers.First.Value.transform.position;
            pivot.transform.rotation = activeGrabbers.First.Value.transform.rotation;
        }
        else
        {
            Vector3 dir = activeGrabbers.First.Value.transform.position - activeGrabbers.Last.Value.transform.position;
            Vector3 mid = (activeGrabbers.First.Value.transform.position + activeGrabbers.Last.Value.transform.position) * 0.5f;
            Vector3 forward = activeGrabbers.First.Value.transform.forward + activeGrabbers.Last.Value.transform.forward;
            pivot.transform.position = mid;
            pivot.transform.rotation = Quaternion.LookRotation(dir, Vector3.Cross(dir, forward));
        }
    }

    protected void DeletePivot()
    {
        if (pivot == null) return;
        Destroy(pivot);
        pivot = null;
    }


}

