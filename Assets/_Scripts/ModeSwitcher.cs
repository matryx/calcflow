using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using NanoVRController;
using Extensions;

[RequireComponent(typeof(RayCastSender))]

public class ModeSwitcher : MonoBehaviour
{

    RayCastSender sender;
    OvrAvatar avatar;
    VRController controller;
    ToggleRayCastOnGrabbable otherToggler;
    Grabber grabber;

    private FreeMarker marker;
    public Transform penPose;
    private GameObject penModel;


    // Use this for initialization
    void Start()
    {
        sender = GetComponent<RayCastSender>();
        avatar = GetComponentInParent<OvrAvatar>();
        grabber = GetComponentInChildren<Grabber>();

        otherToggler = GetComponentInChildren<ToggleRayCastOnGrabbable>();
        controller = gameObject.GetComponent<VRController>();
        marker = gameObject.GetComponentInChildren<FreeMarker>(true);
        penModel = transform.Find("Pen").gameObject;
        ConnectController(controller);
    }

    void ConnectController(VRController c)
    {
        c.components[ButtonId.BUTTON1].ComponentPressed += ToggleMode;
    }

    bool penMode = false;
    void ToggleMode(VRController c, ControllerComponentArgs e)
    {
        if (penMode)
        {
            SwitchToRayCastMode();
        }
        else
        {
            SwitchToPenMode();
        }
        penMode = !penMode;
    }


    void SwitchToPenMode()
    {
        penModel.SetActive(true);

        marker.ConnectController();
        grabber.DisconnectController();

        sender.enabled = false;
        grabber.enabled = false;
        otherToggler.enabled = false;

        SetPenPose();
    }

    void SetPenPose()
    {
        if (avatar)
        {
            if (controller.hand == Handedness.LEFT)
            {
                avatar.LeftHandCustomPose = penPose;
            }
            else
            {
                avatar.RightHandCustomPose = penPose;
            }
        }
    }

    void SetDefaultMode()
    {
        if (avatar)
        {
            if (controller.hand == Handedness.LEFT)
            {
                avatar.LeftHandCustomPose = null;
            }
            else
            {
                avatar.RightHandCustomPose = null;
            }
        }
    }

    void SwitchToRayCastMode()
    {
        marker.DisconnectController();
        grabber.ConnectController();

        sender.enabled = true;
        grabber.enabled = true;
        otherToggler.enabled = true;

        penModel.SetActive(false);
        SetDefaultMode();
    }
}
