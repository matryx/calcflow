using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class OutputManager : MonoBehaviour {
    ParametricManager paramManager;
    public static OutputManager _instance;

    private void Awake()
    {
        _instance = this;
    }

    public void Initialize(ParametricManager pm)
    {
        paramManager = pm;
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
                paramManager.SetOutput(paramManager.expressionSet.GetExpression("X"));
                break;
            case "Button_Yinput":
                paramManager.SetOutput(paramManager.expressionSet.GetExpression("Y"));
                break;
            case "Button_Zinput":
                paramManager.SetOutput(paramManager.expressionSet.GetExpression("Z"));
                break;
            case "Min":
                paramManager.SetOutput(paramManager.expressionSet.GetRange(rangeTitle).Min); 
                break;
            case "Max":
                paramManager.SetOutput(paramManager.expressionSet.GetRange(rangeTitle).Max); 
                break;
            case "GenerateMesh":
                paramManager.toExport = true;
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
