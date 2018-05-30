using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class OutputMenu : MonoBehaviour
{
    [SerializeField]
    SecondaryMenu secondaryMenu;

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        OutputMenu outputMenu;
        internal KeyboardInputResponder(OutputMenu outputMenu)
        {
            this.outputMenu = outputMenu;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            outputMenu.HandleInput(sender.name);
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
                saveButton.SetState(0);
            }
            else if (value == false)
            {
                saveButton.SetState(-1);
            }
            saveable = value;
        }
    }

    FlexActionableComponent saveButton;

    public FlexMenu menu;

    CalcManager calcManager;

    public void Initialize(CalcManager cm)
    {
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        menu.RegisterResponder(responder);

        calcManager = cm;
        saveButton = transform.Find("ControlPanel/Save").GetComponent<FlexActionableComponent>();
    }

    protected void HandleInput(string source)
    {
        ExpressionSet es = calcManager.expressionSet;

        switch (source)
        {
            default:
                print("unknown input: " + source);
                break;
            case "Button_Xinput":
                calcManager.SetOutput(es.GetExpression("X"));
                break;
            case "Button_Yinput":
                calcManager.SetOutput(es.GetExpression("Y"));
                break;
            case "Button_Zinput":
                calcManager.SetOutput(es.GetExpression("Z"));
                break;
            case "umin":
                calcManager.SetOutput(es.GetRange("u").Min);
                break;
            case "umax":
                calcManager.SetOutput(es.GetRange("u").Max);
                break;
            case "tmin":
                calcManager.SetOutput(es.GetRange("t").Min);
                break;
            case "tmax":
                calcManager.SetOutput(es.GetRange("t").Max);
                break;
            case "vmin":
                calcManager.SetOutput(es.GetRange("v").Min);
                break;
            case "vmax":
                calcManager.SetOutput(es.GetRange("v").Max);
                break;
            case "wmin":
                calcManager.SetOutput(es.GetRange("w").Min);
                break;
            case "wmax":
                calcManager.SetOutput(es.GetRange("w").Max);
                break;
            case "GenerateMesh":
                calcManager.toExport = true;
                break;
            case "Save":
                if (Saveable)
                {
                    calcManager.saveLoadMenu.Save();
                }
                break;
            case "Matryx":
                secondaryMenu.gameObject.SetActive(true);
                break;
        }
        calcManager.manageText();
    }

    private void Update()
    {
        Saveable = !calcManager.paramSurface.isGraphing();
    }

}
