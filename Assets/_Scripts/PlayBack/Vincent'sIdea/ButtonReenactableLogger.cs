using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ButtonReenactableLogger : ReenactableLogger
{

    protected override ReenactableType type
    {
        get
        {
			return ButtonReenactableType._instance;
        }
    }

}
