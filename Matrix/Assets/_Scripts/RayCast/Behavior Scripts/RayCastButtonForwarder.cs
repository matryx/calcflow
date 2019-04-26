using CalcFlowUI;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
public class RayCastButtonForwarder : MonoBehaviour
{

    public Button target;
    // Use this for initialization

    void Start()
    {
        RayCastButton source = GetComponent<RayCastButton>();
        if (source != null)
        {
            source.OnButtonEnter += target.PressButton;
            source.OnButtonExit += target.UnpressButton;
        }
    }
}
