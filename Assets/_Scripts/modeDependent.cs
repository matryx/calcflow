using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using NanoVRController;

public class modeDependent : MonoBehaviour {
    VRController controller;
    public GameObject poker;
    public GameObject grabber;
    // Use this for initialization
    void Start () {
        controller = GetComponent<ViveVRController>();
	}
	
	// Update is called once per frame
	void Update () {
		if (controller.pokeMode)
        {
            grabber.SetActive(false);
            poker.SetActive(true);
        } else
        {
            grabber.SetActive(true);
            poker.SetActive(false);
        }
    }
}
