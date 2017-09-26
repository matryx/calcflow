//using UnityEngine;
//using System.Collections;

//public class MainRoomInteraction : MonoBehaviour {

//    public ControllerEventManager controllers;

//    protected GrabbableObject leftActive, rightActive;


//    // Use this for initialization
//    protected virtual void Start () {
//    }

//    protected virtual void OnEnable()
//    { 
       
//        controllers.LeftTriggerPressed += SelectObject;
//        controllers.LeftTriggerUnpressed += ReleaseObject;
//        controllers.RightTriggerPressed += SelectObject;
//        controllers.RightTriggerUnpressed += ReleaseObject;

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

//        controllers.LeftGripPressed -= EnableSelector;
//        controllers.LeftGripUnpressed -= DisableSelector;
//        controllers.RightGripPressed -= EnableSelector;
//        controllers.RightGripUnpressed -= DisableSelector;
//    }

//    // Update is called once per frame
//    protected virtual void Update () {
	
//	}

//    void SelectObject(object sender, ControllerEventArgs e)
//    {
//        //Transform tip = e.controller.GetComponent<InitAttachment>().tip;
//        Transform tip = e.controller.transform.FindChild("Hand").FindChild("GrabVolumeSmall");
//        Collider[] hits = Physics.OverlapSphere(tip.position, .7f);
//        GrabbableObject g = null;
//        foreach (Collider c in hits)
//        {
//            if (c.GetComponent<GrabbableObject>() != null)
//            {
//                g = c.GetComponent<GrabbableObject>();
//                g.RegisterController(e.controller);
//                break;
//            }
//            if (c.transform.parent && c.GetComponentInParent<GrabbableObject>() != null)
//            {
//                g = c.transform.GetComponentInParent<GrabbableObject>();
//                g.RegisterController(e.controller);
//                break;
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
//        else if(!e.isLeft && rightActive)
//        {
//            rightActive.ReleaseController(e.controller);
//            rightActive = null;
//        }
//    }

//    void EnableSelector(object sender, ControllerEventArgs e)
//    {
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
