using CalcFlowUI;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]
public class TouchButton : Button {


    void OnTriggerEnter(Collider other)
    {
        if (other.gameObject.layer == LayerMask.NameToLayer("ButtonPresser")) 
            PressButton(other.gameObject);
    }

    void OnTriggerExit(Collider other)
    {
        if (other.gameObject.layer == LayerMask.NameToLayer("ButtonPresser")) 
            UnpressButton(other.gameObject);
    }

}
