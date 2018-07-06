using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ButtonReenactableUnpress : ReenactableAction
{

    public override string key
    {
        get
        {
            return "buttonUnpress";
        }
    }

    public override void Reenact()
    {
        simulateButtonUnpress();
    }

    private void simulateButtonUnpress()
    {

    }
}
