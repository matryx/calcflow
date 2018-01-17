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

    public void HandleInput(string source, string rangeTitle)
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
            case "Min":
                calcManager.SetOutput(calcManager.expressionSet.ranges[rangeTitle].Min); 
                break;
            case "Max":
                calcManager.SetOutput(calcManager.expressionSet.ranges[rangeTitle].Max); 
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
