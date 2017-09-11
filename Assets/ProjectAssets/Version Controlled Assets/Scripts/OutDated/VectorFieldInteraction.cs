//using UnityEngine;
//using System.Collections;

//public class VectorFieldInteraction : MonoBehaviour
//{

//    public ControllerEventManager controllers;

//    //private [] grabables;

//    protected GrabbableObject leftActive, rightActive;

//    public Transform homePortalPrefab;
//    protected Transform homePortal;

//    // Use this for initialization
//    protected virtual void Start()
//    {
//        //grabables = FindObjectsOfType<ScalableObject>();
        
//    }

//    protected virtual void OnEnable()
//    {
//        controllers.LeftTriggerPressed += SelectObject;
//        controllers.LeftTriggerUnpressed += ReleaseObject;
//        controllers.RightTriggerPressed += SelectObject;
//        controllers.RightTriggerUnpressed += ReleaseObject;

//        controllers.RightMenuPressed += TogglePortal;
//        controllers.LeftMenuPressed += TogglePortal;

//        controllers.LeftGripPressed += EnableSelector;
//        controllers.LeftGripUnpressed += DisableSelector;
//        controllers.RightGripPressed += EnableSelector;
//        controllers.RightGripUnpressed += DisableSelector;
//    }

//    protected virtual void OnDisable()
//    {
//        controllers.LeftTriggerPressed -= SelectObject;
//        controllers.LeftTriggerUnpressed -= ReleaseObject;
//        controllers.RightTriggerPressed -= SelectObject;
//        controllers.RightTriggerUnpressed -= ReleaseObject;

//        controllers.RightMenuPressed -= TogglePortal;
//        controllers.LeftMenuPressed -= TogglePortal;

//        controllers.LeftGripPressed -= EnableSelector;
//        controllers.LeftGripUnpressed -= DisableSelector;
//        controllers.RightGripPressed -= EnableSelector;
//        controllers.RightGripUnpressed -= DisableSelector;
//    }

//    // Update is called once per frame
//    protected virtual void Update()
//    {

//    }

//    void SelectObject(object sender, ControllerEventArgs e)
//    {
//        //Transform tip = e.controller.GetComponent<InitAttachment>().tip;
//        Transform tip = e.controller.transform.FindChild("Hand").FindChild("GrabVolumeSmall");
//        Collider[] hits = Physics.OverlapSphere(tip.position, .5f);
//        GrabbableObject g = null;
//        foreach (Collider c in hits)
//        {
//            if (c.GetComponent<GrabbableObject>() != null)
//            {
//                g = c.GetComponent<GrabbableObject>();
//                g.RegisterController(e.controller);
//                break;
//            }
//            if (c.transform.parent && c.GetComponentInParent<GrabbableObject>()!=null)
//            {
//                g = c.transform.GetComponentInParent<GrabbableObject>();
//                g.RegisterController(e.controller);
//                break;
//            }
//        }
//        if (g != null)
//        {
//            if (e.isLeft)
//            {
//                leftActive = g;
//                controllers.isLeftGrabbing = true;
//            }
//            else
//            {
//                rightActive = g;
//                controllers.isRightGrabbing = true;
//            }
//        }
//    }

//    void ReleaseObject(object sender, ControllerEventArgs e)
//    {
//        if (e.isLeft && leftActive)
//        {
//            leftActive.ReleaseController(e.controller);
//            leftActive = null;
//        }
//        else if (!e.isLeft && rightActive)
//        {
//            rightActive.ReleaseController(e.controller);
//            rightActive = null;
//        }
//    }

//    void TogglePortal(object sender, ControllerEventArgs e)
//    {
//        if (homePortal == null)
//        {
//            Transform tip = e.controller.transform.FindChild("Hand").FindChild("GrabVolumeSmall");
//            homePortal = (Transform)Instantiate(homePortalPrefab, tip.position, Quaternion.identity);
//        }
//        else
//        {
//            Destroy(homePortal.gameObject);
//            homePortal = null;
//        }
//    }

//    void EnableSelector(object sender, ControllerEventArgs e)
//    {
//        Debug.Log("grip");
//        if (!e.isLeft)
//        {
//            e.controller.transform.Find("Hand").Find("r_hand_skeletal").Find("hands:r_hand_world").
//                Find("hands:b_r_hand").Find("hands:b_r_index1").Find("hands:b_r_index2").Find("hands:b_r_index3").Find("coll_hands:b_r_index3").GetComponent<CapsuleCollider>().enabled = true;
//        }
//        else
//        {
//            e.controller.transform.Find("Hand").Find("l_hand_skeletal").Find("hands:l_hand_world").
//                Find("hands:b_l_hand").Find("hands:b_l_index1").Find("hands:b_l_index2").Find("hands:b_l_index3").Find("coll_hands:b_l_index3").GetComponent<CapsuleCollider>().enabled = true;
//        }
//    }

//    void DisableSelector(object sender, ControllerEventArgs e)
//    {
//        if (!e.isLeft)
//        {
//            e.controller.transform.Find("Hand").Find("r_hand_skeletal").Find("hands:r_hand_world").
//                Find("hands:b_r_hand").Find("hands:b_r_index1").Find("hands:b_r_index2").Find("hands:b_r_index3").Find("coll_hands:b_r_index3").GetComponent<CapsuleCollider>().enabled = false;
//        }
//        else
//        {
//            e.controller.transform.Find("Hand").Find("l_hand_skeletal").Find("hands:l_hand_world").
//                Find("hands:b_l_hand").Find("hands:b_l_index1").Find("hands:b_l_index2").Find("hands:b_l_index3").Find("coll_hands:b_l_index3").GetComponent<CapsuleCollider>().enabled = false;
//        }
//    }
//}