using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ToggleFlexPanel : FlexPanelComponent
{

    public FlexActionableComponent[] defaultSettings;

    protected override void OnActionEnd(FlexActionableComponent sender, GameObject collider)
    {
    }

    protected override void OnActionStart(FlexActionableComponent sender, GameObject collider)
    {
        if (sender.State == 2)
        {
            sender.State = 0;
        }
        else if (sender.State == 0)
        {
            sender.SetState(2);
        }
    }

    // Use this for initialization
    void Start()
    {

        if (defaultSettings != null)
        {
            foreach (FlexActionableComponent action in defaultSettings)
            {
                action.SetState(2);
            }
        }
        Actions.Clear();
        AddAllChildActions();
    }
}
