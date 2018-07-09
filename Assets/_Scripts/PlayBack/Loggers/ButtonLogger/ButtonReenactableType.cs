using System.Collections.ObjectModel;
using System.Collections.Generic;
using System.Collections;
using System.Reflection;
using System;
using Extensions;
using CalcFlowUI;
using UnityEngine;

public class ButtonReenactableType : ReenactableType
{
    protected override ReenactableAction[] Actions { get { return reenactors; } }
    protected override Type TargetType { get { return typeof(Button); } }

    private ReenactableAction[] reenactors = {
        new ButtonReenactablePress(),
        new ButtonReenactableUnpress(),
    };
    public ButtonReenactableType()
    {

    }

    protected override void AddLogger(GameObject go)
    {
        go.EnsureOneOf<ButtonReenactableLogger>();
    }


}
