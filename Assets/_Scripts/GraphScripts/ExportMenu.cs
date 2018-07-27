using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExportMenu : MonoBehaviour
{

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        ExportMenu exportMenu;
        internal KeyboardInputResponder(ExportMenu exportMenu)
        {
            this.exportMenu = exportMenu;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            exportMenu.HandleInput(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }

    }


    //PRESTREAM CODE
    bool saveable = true;
    bool Saveable
    {
        get
        {
            return saveable;
        }
        set
        {
            if (saveable == false && value == true)
            {
                if (objButton != null)
                {
                    objButton.SetState(0);
                }
                if (stlButton != null)
                {
                    stlButton.SetState(0);
                }
            }
            else if (value == false)
            {
                if (objButton != null)
                {
                    objButton.SetState(-1);
                }
                if (stlButton != null)
                {
                    stlButton.SetState(-1);
                }
            }
            saveable = value;
        }
    }

    FlexActionableComponent objButton;
    FlexActionableComponent stlButton;

    public FlexMenu menu;

    SurfaceTessellation tessel;
    public void Initialize(SurfaceTessellation st)
    {
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        menu.RegisterResponder(responder);

        tessel = st;
        objButton = transform.Find("ControlPanel/ExportObj").GetComponent<FlexActionableComponent>();
        stlButton = transform.Find("ControlPanel/ExportStl").GetComponent<FlexActionableComponent>();
    }

    protected void HandleInput(string source)
    {
        switch (source)
        {
            default:
                print("unknown input: " + source);
                break;
            case "ExportObj":
                tessel.ExportAsObj();
                break;
            case "ExportStl":
                //if (Saveable)
                //{
                tessel.ExportAsStl();
                //}
                break;
        }
    }

    private void Update()
    {
    }

}
