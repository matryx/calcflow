using System.Collections;
using System.Collections.Generic;
using UnityEngine;


/*
 * This is a basic FlexPanel that will highlight clicked buttons until another button is clicked.
 */

public class MultiSelectFlexPanel : FlexPanelComponent
{

    public bool MultiSelect;
    public Dictionary<string, GameObject> selected = new Dictionary<string, GameObject>();
    public Color selectedUnselectedColor;
    public Color selectedSelectedColor;
    public Color hoverUnselectedColor;
    public Color hoverSelectedColor;
    public Color passiveUnselectedColor;
    public Color passiveSelectedColor;

    public void SwitchToMultiSelect()
    {
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
                ((FlexButtonComponent)button).SetState(2);
                ((FlexButtonComponent)button).SetState(0);
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
                ((FlexButtonComponent)button).SetState(2);
                ((FlexButtonComponent)button).SetState(0);
            }
        }
    }

    public void ChangePassiveColor(Color newColor)
    {
        foreach (FlexActionableComponent button in Actions)
        {
            if (button is FlexButtonComponent)
            {
                ((FlexButtonComponent)button).passiveColor = newColor;
                ((FlexButtonComponent)button).SetState(2);
                ((FlexButtonComponent)button).SetState(0);
            }
        }
    }

    
    public void ChangeSelectedColor(FlexButtonComponent button, Color newColor)
    {
       button.selectedColor = newColor;
    }

    public void ChangeHoveringColor(FlexButtonComponent button, Color newColor)
    {
        button.hoveringColor = newColor;
    }
    
    public void ChangePassiveColor(FlexButtonComponent button, Color newColor)
    {
        button.passiveColor = newColor;
    }

    public new void ClearSelection()
    {
        ChangePassiveColor(passiveUnselectedColor);
        ChangeHoveringColor(hoverUnselectedColor);
        ChangeSelectedColor(selectedUnselectedColor);
        base.ClearSelection();
        selected.Clear();
    }

    protected override void StartAction(FlexActionableComponent sender, GameObject collider)
    {
        if (menu != null)
        {
            if(MultiSelect)
            {
                if (selected.ContainsKey(sender.name))
                {
                    selected.Remove(sender.name);
                    ChangeSelectedColor((FlexButtonComponent)sender, selectedUnselectedColor);
                    ChangeHoveringColor((FlexButtonComponent)sender, hoverUnselectedColor);
                    ChangePassiveColor((FlexButtonComponent)sender, passiveUnselectedColor);
                }
                else
                {
                    selected.Add(sender.name, sender.gameObject);
                    ChangeSelectedColor((FlexButtonComponent)sender, selectedSelectedColor);
                    ChangeHoveringColor((FlexButtonComponent)sender, hoverSelectedColor);
                    ChangePassiveColor((FlexButtonComponent)sender, passiveSelectedColor);
                }
            }
            
            menu.StartAction(name, sender, collider);
            OnActionStart(sender, collider);
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
            selectedUnselectedColor = QuickButton.TOGGLE_OFF;
            selectedSelectedColor = QuickButton.TOGGLE_ON;
            hoverUnselectedColor = QuickButton.LIGHT_PASSIVE;
            hoverSelectedColor = QuickButton.LIGHT_HOVERING;
            passiveUnselectedColor = Color.white;
            passiveSelectedColor = QuickButton.DARK_PASSIVE;

            AddAllChildActions();
            initialized = true;
        }
        return this;
    }
}
