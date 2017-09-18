using NanoVRController;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(RayCastSender))]

public class RayCastGrabber : MonoBehaviour {

    Grabber rayGrabber;
    public VRController controller;
    RayCastSender sender;

    void Start()
    {
        sender = GetComponent<RayCastSender>();
        InitRayGrabber();
        ConnectController();
    }

    void Enable()
    {
        ConnectController();
    }

    void Disable()
    {
        Drop();
        DisconnectController();
    }


    public void ConnectController()
    {
        controller.components[ButtonId.GRIP].ComponentPressed += Grab;
        controller.components[ButtonId.GRIP].ComponentUnpressed += Drop;
        controller.components[ButtonId.THUMBPAD].ComponentTouching += PushSelected;
        controller.components[ButtonId.THUMBPAD].ComponentTouching += RotateSelected;
    }

    public void DisconnectController()
    {
        controller.components[ButtonId.GRIP].ComponentPressed += Grab;
        controller.components[ButtonId.GRIP].ComponentUnpressed += Drop;
        controller.components[ButtonId.THUMBPAD].ComponentTouching += PushSelected;
        controller.components[ButtonId.THUMBPAD].ComponentTouching += RotateSelected;
    }

    private void InitRayGrabber()
    {
        rayGrabber = new GameObject().AddComponent<Grabber>();
        rayGrabber.SetController(controller);
        rayGrabber.name = "raygrabber";
        rayGrabber.transform.parent = this.transform;
    }

    void Grab(VRController c, ControllerComponentArgs e)
    {
        Grab();
    }

    void Grab()
    {
        if (!sender.CurrTargetData.hitting) return;
        //Grippable grippable = sender.CurrTargetData.target.GetComponentInParent<Grippable>();
        Grippable grippable = sender.CurrTargetData.target.GetComponent<Grippable>();
        if (!grippable) return;
        rayGrabber.transform.position = sender.TargetPoint;
        rayGrabber.GrabObject(grippable);
    }

    void Drop()
    {
        if (rayGrabber)
        {
            rayGrabber.DropAll();
        }
    }

    void Drop(VRController c, ControllerComponentArgs e)
    {
        Drop();
    }

    public float speed;

    void PushSelected(VRController c, ControllerComponentArgs e)
    {
        if (Mathf.Abs(e.x) > Mathf.Abs(e.y)) return;

        Vector3 currPos = rayGrabber.transform.position;
        Vector3 nextPos = currPos + transform.forward * e.y * speed;

        float angle = Vector3.Angle(transform.forward, nextPos - transform.position);


        if (Mathf.Abs(angle) < 90)
        {
            rayGrabber.transform.position = nextPos;
        }
    }

    void RotateSelected(VRController c, ControllerComponentArgs e)
    {
        if (Mathf.Abs(e.y) > Mathf.Abs(e.x)) return;

        rayGrabber.transform.RotateAround(rayGrabber.transform.position, Vector3.up, e.x * 4);
    }
}
