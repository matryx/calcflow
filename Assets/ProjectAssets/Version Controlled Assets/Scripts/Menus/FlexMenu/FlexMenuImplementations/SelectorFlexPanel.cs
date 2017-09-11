using System.Collections;
using System.Collections.Generic;
using UnityEngine;


/*
 * This is a basic FlexPanel that will highlight clicked buttons until another button is clicked.
 */

public class SelectorFlexPanel : FlexPanelComponent
{
    public FlexActionableComponent defaultSetting;

    protected override void OnActionEnd(FlexActionableComponent sender, GameObject collider)
    {
    }

    protected override void OnActionStart(FlexActionableComponent sender, GameObject collider)
    {
        ClearSelection();
        sender.SetState(2);
    }

    // Use this for initialization
    void Start()
    {
        if(defaultSetting != null)
        {
            defaultSetting.SetState(2);
        }
        Actions.Clear();
        AddAllChildActions();
    }
}
