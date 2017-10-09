using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(RayCastSender))]
[RequireComponent(typeof(Grabber))]

public class ToggleRayCastOnGrabbable : MonoBehaviour {

    Grabber handGrabber;
    RayCastSender rayCastSender;

    // Use this for initialization
    void Start () {
        handGrabber = GetComponent<Grabber>();
        rayCastSender = GetComponent<RayCastSender>();
	}
	


	// Update is called once per frame
	void Update () {
        if (handGrabber.CanGrab())
        {
            rayCastSender.enabled = false;
        } else
        {
            rayCastSender.enabled = true;
        }
    }
}
