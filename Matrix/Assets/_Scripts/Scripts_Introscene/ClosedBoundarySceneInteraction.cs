using UnityEngine;
using System.Collections;

public class ClosedBoundarySceneInteraction : MonoBehaviour {

    public ControllerEventManager controllers;

    //private GrabableObject[] grabables;

    GrabableObject leftActive, rightActive;

    public Transform homePortalPrefab;
    Transform homePortal;

    // Use this for initialization
    void Start()
    {
        //grabables = FindObjectsOfType<GrabableObject>();
    }

    void OnEnable()
    {
        controllers.LeftTriggerPressed += SelectObject;
        controllers.LeftTriggerHolding += GrabObject;
        controllers.LeftTriggerUnpressed += ReleaseObject;
        controllers.RightTriggerPressed += SelectObject;
        controllers.RightTriggerHolding += GrabObject;
        controllers.RightTriggerUnpressed += ReleaseObject;

        controllers.RightMenuPressed += TogglePortal;
        controllers.LeftMenuPressed += TogglePortal;
    }

    void OnDisable()
    {
        controllers.LeftTriggerPressed -= SelectObject;
        controllers.LeftTriggerHolding -= GrabObject;
        controllers.LeftTriggerUnpressed -= ReleaseObject;
        controllers.RightTriggerPressed -= SelectObject;
        controllers.RightTriggerHolding -= GrabObject;
        controllers.RightTriggerUnpressed -= ReleaseObject;

        controllers.RightMenuPressed -= TogglePortal;
        controllers.LeftMenuPressed -= TogglePortal;
    }

    // Update is called once per frame
    void Update()
    {

    }

    void SelectObject(object sender, ControllerEventArgs e)
    {
        //Transform tip = e.controller.GetComponent<InitAttachment>().tip;
        Transform tip = e.controller.transform.Find("Custom_Model");
        Collider[] hits = Physics.OverlapSphere(tip.position, .5f);
        //GrabableObject g = null;
        foreach (Collider c in hits)
        {
            if (c.GetComponent<GrabableObject>() != null)
            {
                //g = c.GetComponent<GrabableObject>();
                break;
            }
            if (c.transform.parent && c.transform.parent.GetComponent<GrabableObject>() != null)
            {
                //g = c.transform.parent.GetComponent<GrabableObject>();
                break;
            }
        }
        //if (g != null)
        //{
        //    if (e.isLeft)
        //    {
        //        leftActive = g;
        //        controllers.GetComponent<ControllerAnimation>().isLeftGrabbing = true;
        //    }
        //    else
        //    {
        //        rightActive = g;
        //        controllers.GetComponent<ControllerAnimation>().isRightGrabbing = true;
        //    }
        //}

        /*
        foreach (GrabableObject g in grabables)
        {
            if((g.transform.position - tip.position).magnitude < 0.3f)
            {
                if (e.isLeft)
                {
                    leftActive = g;
                    controllers.GetComponent<ControllerAnimation>().isLeftGrabbing = true;
                }
                else
                {
                    rightActive = g;
                    controllers.GetComponent<ControllerAnimation>().isRightGrabbing = true;
                }
            }
        }
        */
    }

    void GrabObject(object sender, ControllerEventArgs e)
    {
        //Transform tip = e.controller.GetComponent<InitAttachment>().tip;
        Transform tip = e.controller.transform.Find("Custom_Model");
        if (e.isLeft && leftActive != null)
        {
            leftActive.MoveTowards(tip.position);
        }
        else if (!e.isLeft && rightActive != null)
        {
            rightActive.MoveTowards(tip.position);
        }
    }

    void ReleaseObject(object sender, ControllerEventArgs e)
    {
        if (e.isLeft && leftActive != null)
        {
            if (leftActive.GetComponent<Rigidbody>() != null)
            {
                leftActive.GetComponent<Rigidbody>().velocity = SteamVR_Controller.Input((int)e.controller.index).velocity;
                leftActive.GetComponent<Rigidbody>().angularVelocity = SteamVR_Controller.Input((int)e.controller.index).angularVelocity;
            }
            leftActive.Release();
            leftActive = null;
            //controllers.GetComponent<ControllerAnimation>().isLeftGrabbing = false;
        }
        else if (!e.isLeft && rightActive != null)
        {
            if (rightActive.GetComponent<Rigidbody>() != null)
            {
                rightActive.GetComponent<Rigidbody>().velocity = SteamVR_Controller.Input((int)e.controller.index).velocity;
                rightActive.GetComponent<Rigidbody>().angularVelocity = SteamVR_Controller.Input((int)e.controller.index).angularVelocity;
            }
            rightActive.Release();
            rightActive = null;
            //controllers.GetComponent<ControllerAnimation>().isRightGrabbing = false;
        }
    }

    void TogglePortal(object sender, ControllerEventArgs e)
    {
        if (homePortal == null)
        {
            Transform tip = e.controller.transform.Find("Custom_Model");
            homePortal = (Transform)Instantiate(homePortalPrefab, tip.position, Quaternion.identity);
        }
        else
        {
            Destroy(homePortal.gameObject);
            homePortal = null;
        }
    }
}
