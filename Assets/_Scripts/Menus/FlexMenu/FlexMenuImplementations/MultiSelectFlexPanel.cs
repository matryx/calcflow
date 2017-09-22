using System.Collections;
using System.Collections.Generic;
using UnityEngine;


/*
 * This is a basic FlexPanel that will highlight clicked buttons until another button is clicked.
 */

public class MultiSelectFlexPanel : FlexPanelComponent
{

    public bool MultiSelect;

    public void SwitchToMultiSelect()
    {
        ClearSelection();
        MultiSelect = true;
    }

    public void SwitchToSingleSelect()
    {
        ClearSelection();
        MultiSelect = false;
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

    public void ChangeHoveringColor(Color newColor)
    {
        foreach (FlexActionableComponent button in Actions)
        {
            if (button is FlexButtonComponent)
            {
                ((FlexButtonComponent)button).hoveringColor = newColor;
            }
        }
    }

    protected override void OnActionStart(FlexActionableComponent sender, GameObject collider)
    {
        if (MultiSelect)
        {
            if (sender.State == 2)
            {
                sender.SetState(1);
            }
            else
            {
                sender.SetState(2);
            }
        }
        else
        {
            sender.SetState(2);
        }
    }

    protected override void OnActionEnd(FlexActionableComponent sender, GameObject collider)
    {
        if (MultiSelect)
        {
        }
        else
        {
            sender.SetState(0);
        }

    }

    bool initialized = false;

    private void Start()
    {
        Initialize();
    }
    // Use this for initialization
    public MultiSelectFlexPanel Initialize()
    {
        if (!initialized)
        {
            AddAllChildActions();
            initialized = true;
        }
        return this;
    }
}
