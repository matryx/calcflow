using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using NanoVRController;

public class ToggleObject : MonoBehaviour {

    public VRController controller;
    public ButtonId button;
    public GameObject gObj;

    private void Start()
    {
        controller.components[button].ComponentPressed += toggle;
    }

    public void toggle(VRController c, ControllerComponentArgs e)
    {
        gObj.SetActive(!gObj.activeSelf);
    }
}
