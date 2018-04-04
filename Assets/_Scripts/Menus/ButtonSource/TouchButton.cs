using CalcFlowUI;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

public class TouchButton : Button {

    //public bool verbose = false;

    void OnTriggerEnter(Collider other)
    {
        if (verbose)
        {
            print("triggered");
        }
        if (other.gameObject.layer == LayerMask.NameToLayer("ButtonPresser")) 
            PressButton(other.gameObject);
    }

    void OnTriggerExit(Collider other)
    {
        if (other.gameObject.layer == LayerMask.NameToLayer("ButtonPresser")) 
            UnpressButton(other.gameObject);
    }

}
