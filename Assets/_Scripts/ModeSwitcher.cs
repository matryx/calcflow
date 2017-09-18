using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using NanoVRController;

[RequireComponent(typeof(RayCastSender))]

public class ModeSwitcher : MonoBehaviour {

    RayCastSender sender;
    OvrAvatar avatar;
    VRController controller;
    ToggleRayCastOnGrabbable otherToggler;
    Grabber grabber;

    public FreeMarker marker;
    public Transform penPose;
    public GameObject penModel;


    // Use this for initialization
    void Start () {
        sender = GetComponent<RayCastSender>();
        avatar = GetComponentInParent<OvrAvatar>();
        grabber = GetComponentInParent<Grabber>();

        otherToggler = GetComponentInParent<ToggleRayCastOnGrabbable>();
        controller = marker.controller;
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
        } else
        {
            SwitchToPenMode();
        }
        penMode = !penMode;
    }


    void SwitchToPenMode()
    {
        marker.ConnectController();
        grabber.DisconnectController();

        sender.enabled = false;
        grabber.enabled = false;
        otherToggler.enabled = false;

        penModel.SetActive(true);
        if (controller.hand == Handedness.LEFT)
        {
            avatar.LeftHandCustomPose = penPose;
        } else
        {
            avatar.RightHandCustomPose = penPose;
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
