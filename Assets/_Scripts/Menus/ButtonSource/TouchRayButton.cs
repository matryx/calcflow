using CalcFlowUI;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
[RequireComponent(typeof(RayCastButton))]
[RequireComponent(typeof(TouchButton))]

[ExecuteInEditMode]
public class TouchRayButton : MonoBehaviour {

    // Use this for initialization
    void Start () {

    }


    private void Update()
    {
        DestroyImmediate(this);
    }
}
