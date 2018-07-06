using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ButtonReenactableType : ReenactableType<ButtonReenactableLogger>
{
    public static ButtonReenactableType _instance = new ButtonReenactableType();
    protected override ReenactableAction[] Actions { get { return reenactors; } }

    private ReenactableAction[] reenactors = {
        new ButtonReenactablePress(),
        new ButtonReenactableUnpress(),
    };
    public ButtonReenactableType()
    {

    }

}
