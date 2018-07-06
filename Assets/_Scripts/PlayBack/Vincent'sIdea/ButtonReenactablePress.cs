using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ButtonReenactablePress : ReenactableAction
{

    public override string key
    {
        get
        {
            return "buttonPress";
        }
    }
    public override void Reenact()
    {
        simulateButtonPress();
    }

    private void simulateButtonPress()
    {

    }

}
