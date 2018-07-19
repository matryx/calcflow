using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/*
 * This is a basic FlexPanel that will highlight buttons only as long as they are being pressed. 
 */


public class KeyboardFlexPanel : FlexPanelComponent {

    protected override void OnActionEnd(FlexActionableComponent sender, GameObject collider)
    {
        Debug.Log("Flex End");
        sender.SetState(0);
    }

    protected override void OnActionStart(FlexActionableComponent sender, GameObject collider)
    {
        Debug.Log("Flex Start");
        sender.SetState(2);
    }

    // Use this for initialization
    void Start()
    {
        Actions.Clear();
        AddAllChildActions();
    }
}
