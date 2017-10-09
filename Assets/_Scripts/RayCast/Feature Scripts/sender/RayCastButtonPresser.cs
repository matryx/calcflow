using CalcFlowUI;
using Extensions;
using NanoVRController;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(RayCastSender))]

public class RayCastButtonPresser : MonoBehaviour {

    public VRController controller;
    RayCastSender sender;

    // Use this for initialization
    void Start () {
        sender = GetComponent<RayCastSender>();
        ConnectController();
    }

    Button button;
    void ButtonPress(VRController c, ControllerComponentArgs e)
    {
        if (!sender.CurrTargetData.hitting) return;
        button = sender.CurrTargetData.target.GetComponent<RayCastButton>();
        if (button != null)
        {
            button.PressButton(this.gameObject);
        }
    }

    void ButtonUnpress(VRController c, ControllerComponentArgs e)
    {
        ButtonUnpress();
    }

    void ButtonUnpress()
    {
        if (button != null) button.UnpressButton(this.gameObject);
        button = null;
    }


    public void ConnectController()
    {
        controller.components[ButtonId.TRIGGER].ComponentPressed += ButtonPress;
        controller.components[ButtonId.TRIGGER].ComponentUnpressed += ButtonUnpress;
    }

    public void DisconnectController()
    {
        ButtonUnpress();
        controller.components[ButtonId.TRIGGER].ComponentPressed -= ButtonPress;
        controller.components[ButtonId.TRIGGER].ComponentUnpressed -= ButtonUnpress;
    }

    private void OnDestroy()
    {
        DisconnectController();
    }
}
