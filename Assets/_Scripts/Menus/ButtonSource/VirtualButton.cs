using CalcFlowUI;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]
public class VirtualButton : Button
{
#if UNITY_EDITOR
    public KeyCode debugHotKey;
    protected override void Update()
    {
        base.Update();
        if (Input.GetKeyDown(debugHotKey))
            PressButton(null);
    }

#endif
}
