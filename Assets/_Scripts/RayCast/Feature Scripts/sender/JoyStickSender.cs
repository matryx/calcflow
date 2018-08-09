using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using NanoVRController;

[RequireComponent(typeof(RayCastSender))]


public class JoyStickSender : MonoBehaviour {

    VRController controller;
    RayCastSender sender;

	// Use this for initialization
	void Start () {
        controller = GetComponentInParent<VRController>();
        sender = GetComponentInParent<RayCastSender>();
        ConnectController();
    }

    public void ConnectController()
    {
        controller.components[ButtonId.THUMBPAD].ComponentTouching += SendJoyStickInput;
    }

    public void DisconnectController()
    {
        controller.components[ButtonId.THUMBPAD].ComponentTouching += SendJoyStickInput;
    }

    void SendJoyStickInput(VRController c, ControllerComponentArgs e)
    {
        if (!sender.CurrTargetData.hitting) return;
        JoyStickReceiver receiver = sender.CurrTargetData.target.GetComponent<JoyStickReceiver>();
        if (receiver != null)
        {
            receiver.ReceiverJoystickInput(c, e);
        }
    }
}
