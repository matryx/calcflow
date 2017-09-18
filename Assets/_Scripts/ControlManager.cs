using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using NanoVRController;

public class ControlManager : MonoBehaviour {
    public VRController controller;
    public ControlScheme FreeMarkerScheme;
    public ControlScheme grabScheme;

    public enum MODE { marker, grab }
    private MODE currMode;

    public MODE mode
    {
        get
        {
            return currMode;
        }
        set
        {
            currMode = value;
            updateMode();
        }
    }

    private void Start()
    {
        currMode = MODE.grab;
        ConnectController();
    }

    private void updateMode()
    {
        switch (currMode)
        {
            case MODE.marker:
                FreeMarkerScheme.Enable();
                grabScheme.Disable();
                break;
            case MODE.grab:
                grabScheme.Enable();
                FreeMarkerScheme.Disable();
                break;
        }
    }  

    void togglePenMode (VRController c, ControllerComponentArgs e)
    {
        if (mode == MODE.marker)
        {
            mode = MODE.grab;
        } else
        {
            mode = MODE.marker;
        }
    }

    private void ConnectController()
    {
        controller.components[ButtonId.BUTTON1].ComponentPressed += togglePenMode;
    }

    private void DisconnectController()
    {
        controller.components[ButtonId.BUTTON1].ComponentPressed -= togglePenMode;
    }

}