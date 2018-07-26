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

    const ExpressionSet.ExpOptions X = ExpressionSet.ExpOptions.X;
    const ExpressionSet.ExpOptions Y = ExpressionSet.ExpOptions.Y;
    const ExpressionSet.ExpOptions Z = ExpressionSet.ExpOptions.Z;

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
        switch (source)
        {
            default:
                print("unknown input: " + source);
                break;
            case "Button_Xinput":
                calcManager.SetOutput(calcManager.expressionSet.expressions[X]);
                break;
            case "Button_Yinput":
                calcManager.SetOutput(calcManager.expressionSet.expressions[Y]);
                break;
            case "Button_Zinput":
                calcManager.SetOutput(calcManager.expressionSet.expressions[Z]);
                break;
            case "umin":
                calcManager.SetOutput(calcManager.expressionSet.ranges["u"].Min);
                break;
            case "umax":
                calcManager.SetOutput(calcManager.expressionSet.ranges["u"].Max);
                break;
            case "tmin":
                calcManager.SetOutput(calcManager.expressionSet.ranges["t"].Min);
                break;
            case "tmax":
                calcManager.SetOutput(calcManager.expressionSet.ranges["t"].Max);
                break;
            case "vmin":
                calcManager.SetOutput(calcManager.expressionSet.ranges["v"].Min);
                break;
            case "vmax":
                calcManager.SetOutput(calcManager.expressionSet.ranges["v"].Max);
                break;
            case "wmin":
                calcManager.SetOutput(calcManager.expressionSet.ranges["w"].Min);
                break;
            case "wmax":
                calcManager.SetOutput(calcManager.expressionSet.ranges["w"].Max);
                break;
            case "GenerateMesh":
                calcManager.toExport = true;
                break;
            case "GenerateStl":
                calcManager.toStl = true;
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
