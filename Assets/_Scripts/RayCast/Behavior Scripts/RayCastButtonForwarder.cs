using CalcFlowUI;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public class RayCastButtonForwarder : MonoBehaviour
{

    public Button target;
    // Use this for initialization

    void Start()
    {
        VirtualButton vSource = GetComponent<VirtualButton>();
        if (vSource != null)
        {
            vSource.OnButtonEnter += target.PressButton;
            vSource.OnButtonExit += target.UnpressButton;
        }
        RayCastButton source = GetComponent<RayCastButton>();
        if (source != null)
        {
            source.OnButtonEnter += target.PressButton;
            source.OnButtonExit += target.UnpressButton;
        }
    }
}
