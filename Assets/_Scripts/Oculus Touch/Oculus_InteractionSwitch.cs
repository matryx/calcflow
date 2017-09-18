using UnityEngine;
using System.Collections;

public class Oculus_InteractionSwitch : MonoBehaviour {

    Oculus_TrackedController controller;
    Oculus_GrabbingInteractions grab;
    Oculus_3DDraw draw;

    void Awake()
    {
        controller = GetComponent<Oculus_TrackedController>();
        grab = GetComponent<Oculus_GrabbingInteractions>();
        draw = GetComponent<Oculus_3DDraw>();
        grab.enabled = true;
        draw.enabled = false;

        controller.button1Pressed += EnableGrabMode;
        controller.button2Pressed += EnableDrawMode;
    }

    void EnableGrabMode(object sender, Oculus_ControllerArgs e)
    {
        if (!grab.enabled)
        {
            grab.enabled = true;
            draw.enabled = false;
        }
    }

    void EnableDrawMode(object sender, Oculus_ControllerArgs e)
    {
        if (!draw.enabled)
        {
            draw.enabled = true;
            grab.enabled = false;
        }
    }
}
