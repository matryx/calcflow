using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/*
 * This is a basic FlexPanel that will highlight buttons only as long as they are being pressed. 
 */


public class KeyboardFlexPanel : FlexPanelComponent {

    protected override void OnActionEnd(FlexActionableComponent sender, GameObject collider)
    {
        sender.SetState(0);
    }

    protected override void OnActionStart(FlexActionableComponent sender, GameObject collider)
    {
        sender.SetState(2);
    }

    public void ChangeSelectedColor(Color newColor)
    {
        foreach (FlexActionableComponent button in Actions)
        {
            if (button is FlexButtonComponent)
            {
                ((FlexButtonComponent)button).selectedColor = newColor;
            }
        }
    }

    // Use this for initialization
    void Start()
    {
        RemoveAllActions();
        AddAllChildActions();
    }
}
