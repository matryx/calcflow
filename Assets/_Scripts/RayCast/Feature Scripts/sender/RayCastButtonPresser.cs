using CalcFlowUI;
using Extensions;
using NanoVRController;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(RayCastSender))]

public class RayCastButtonPresser : MonoBehaviour {

    public setRotation rotator;
    public setPosition pos1, pos2;
    public VRController controller;
    public GameObject exists;
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
            if ((button.name == "Button_Enter" || button.transform.parent.name == "Button_Enter") && exists != null)
            {
                rotator.makechanges = true;
                pos1.makechanges = true;
                pos2.makechanges = true;
            }
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
