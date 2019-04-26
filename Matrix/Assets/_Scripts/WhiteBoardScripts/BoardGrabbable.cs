//using UnityEngine;
//using System.Collections;

//public class BoardGrabbable : GrabbableObject {
//    protected GameObject pivot;
//    float distance;

//    protected override void Start()
//    {
//        base.Start();
//    }

//    protected override void Update()
//    {
//        base.Update();
//        if (registeredControllers.Count == 2)
//        {
//            UpdatePivot();
//        }
//    }

//    public override void ReleaseController(SteamVR_TrackedObject controller)
//    {
//        base.ReleaseController(controller);
//        if (registeredControllers.Count == 0 && GetComponent<Rigidbody>() != null)
//        {
//            var origin = controller.origin ? controller.origin : controller.transform.parent;
//            if (origin == null)
//            {
//                GetComponent<Rigidbody>().velocity = SteamVR_Controller.Input((int)controller.index).velocity;
//                GetComponent<Rigidbody>().angularVelocity = SteamVR_Controller.Input((int)controller.index).angularVelocity;
//            }
//            else
//            {
//                GetComponent<Rigidbody>().velocity = origin.TransformVector(SteamVR_Controller.Input((int)controller.index).velocity);
//                GetComponent<Rigidbody>().angularVelocity = origin.TransformVector(SteamVR_Controller.Input((int)controller.index).angularVelocity);
//            }
//        }
//    }

//    protected override void OnRegisterController()
//    {
//        base.OnRegisterController();
//        if (registeredControllers.Count == 1)
//        {
//            AttachParent(registeredControllers.First.Value.transform);
//        }
//        else if (registeredControllers.Count == 2)
//        {
//            CreatePivot();
//            AttachParent(pivot.transform);
//        }
//    }

//    protected override void OnReleaseController()
//    {
//        base.OnReleaseController();
//        if (registeredControllers.Count == 1)
//        {
//            AttachParent(registeredControllers.First.Value.transform);
//        }
//        else if (registeredControllers.Count == 0)
//        {
//            DetachParent();
//            DeletePivot();
//        }
//    }

//    protected override void DetachParent() {
//        transform.SetParent(null, true);
//    }

//    private void CreatePivot()
//    {
//        pivot = new GameObject();
//        Vector3 dir = registeredControllers.First.Value.transform.position - registeredControllers.Last.Value.transform.position;
//        Vector3 mid = (registeredControllers.First.Value.transform.position + registeredControllers.Last.Value.transform.position) * 0.5f;
//        Vector3 forward = registeredControllers.First.Value.transform.forward + registeredControllers.Last.Value.transform.forward;
//        distance = dir.magnitude;
//        pivot.transform.position = mid;
//        pivot.transform.rotation = Quaternion.LookRotation(dir, Vector3.Cross(dir, forward));
//    }

//    private void UpdatePivot()
//    {
//        Vector3 dir = registeredControllers.First.Value.transform.position - registeredControllers.Last.Value.transform.position;
//        Vector3 mid = (registeredControllers.First.Value.transform.position + registeredControllers.Last.Value.transform.position) * 0.5f;
//        Vector3 forward = registeredControllers.First.Value.transform.forward + registeredControllers.Last.Value.transform.forward;
//        float newDistance = dir.magnitude;
//        pivot.transform.position = mid;
//        pivot.transform.rotation = Quaternion.LookRotation(dir, Vector3.Cross(dir, forward));
//        pivot.transform.localScale *= newDistance / distance;
//        distance = newDistance;
//    }

//    private void DeletePivot()
//    {
//        Destroy(pivot);
//        pivot = null;
//    }
//}

