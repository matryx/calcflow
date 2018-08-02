using CalcFlowUI;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VirtualButton : Button
{
#if UNITY_EDITOR
    public KeyCode debugHotKey;
    protected override void Update()
    {
        if (Input.GetKeyDown(debugHotKey))
            PressButton(null);
    }

#endif
}
