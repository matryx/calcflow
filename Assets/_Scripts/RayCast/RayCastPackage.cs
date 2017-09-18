using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(RayCastSender))]
[RequireComponent(typeof(RayCastButtonPresser))]
[RequireComponent(typeof(RayCastGrabber))]
[RequireComponent(typeof(ToggleRayCastOnGrabbable))]
[RequireComponent(typeof(RayCastRenderer))]

[ExecuteInEditMode]
public class RayCastPackage : MonoBehaviour {

	// Use this for initialization
	void Update () {
        var RCG = GetComponent<RayCastGrabber>();
        RCG.controller = GetComponentInParent<NanoVRController.VRController>();
        RCG.speed = 1;
        var grabber = GetComponent<Grabber>();
        grabber.controller = RCG.controller;
        var RCS = GetComponent<RayCastSender>();
        print(LayerMask.NameToLayer("Avatar"));
        print(LayerMask.NameToLayer("ButtonPresser"));
        RCS.raycastLayers = ~(LayerMask.GetMask("Avatar", "ButtonPresser"));
        DestroyImmediate(this);
	}

}
