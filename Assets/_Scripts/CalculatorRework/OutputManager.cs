using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class OutputManager : MonoBehaviour {
    CalculatorManager calcManager;
    public static OutputManager _instance;

    private void Awake()
    {
        _instance = this;
        calcManager = ParametricManager._instance;
    }

    public void Initialize()
    {
        //saveButton = transform.Find("ControlPanel/Save").GetComponent<FlexActionableComponent>();
    }

    public void setManager(CalculatorManager cm)
    {
        calcManager = cm;
    }

    public void HandleInput(string source, string rangeTitle)
    {
        switch (source)
        {
            default:
                print("unknown input: " + source);
                break;
            case "Button_Xinput":
                calcManager.SetOutput(calcManager.expressionSet.GetExpression("X"));
                break;
            case "Button_Yinput":
                calcManager.SetOutput(calcManager.expressionSet.GetExpression("Y"));
                break;
            case "Button_Zinput":
                calcManager.SetOutput(calcManager.expressionSet.GetExpression("Z"));
                break;
            case "Min":
                calcManager.SetOutput(calcManager.expressionSet.GetRange(rangeTitle).Min); 
                break;
            case "Max":
                calcManager.SetOutput(calcManager.expressionSet.GetRange(rangeTitle).Max); 
                break;
            case "GenerateMesh":
                calcManager.toExport = true;
                break;
            case "Save":
                //calcManager.saveLoadMenu.Save();

                //if (Saveable)
                //{
                //}
                break;
        }
        //calcManager.manageText();
    }

    private void Update()
    {
        //Saveable = !calcManager.paramSurface.isGraphing();
    }
}
