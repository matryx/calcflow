using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class OutputManager : MonoBehaviour {
    const ExpressionSet.ExpOptions X = ExpressionSet.ExpOptions.X;
    const ExpressionSet.ExpOptions Y = ExpressionSet.ExpOptions.Y;
    const ExpressionSet.ExpOptions Z = ExpressionSet.ExpOptions.Z;

    CalculatorManager calcManager;
    public static OutputManager _instance;

    private void Awake()
    {
        _instance = this;
    }

    public void Initialize(CalculatorManager cm)
    {
        calcManager = cm;
        //saveButton = transform.Find("ControlPanel/Save").GetComponent<FlexActionableComponent>();
    }

    public void HandleInput(string source)
    {
        print("HANDLE CALLD");
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
                //case "Save":
                //    if (Saveable)
                //    {
                //        calcManager.saveLoadMenu.Save();
                //    }
                //    break;
        }
        calcManager.manageText();
    }

    private void Update()
    {
        //Saveable = !calcManager.paramSurface.isGraphing();
    }
}
