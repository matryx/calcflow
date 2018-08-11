using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class OutputManager : MonoBehaviour {
    public static OutputManager _instance;

    CalculatorManager calcManager;
    CustomParametrizedSurface paramSurface;
    SaveLoadMenu saveLoadMenu;

    FlexActionableComponent saveButton;

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

    void Awake()
    {
        Initialize();
    }

    public void Initialize()
    {
        _instance = this;
        calcManager = ParametricManager._instance;
        paramSurface = CustomParametrizedSurface._instance;
        saveLoadMenu = SaveLoadMenu._instance;

        //TODO: initialize saveButton
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
                //TODO: debug null

                if (calcManager == null)
                {
                    print("CALC MANAGER NULL");
                }
                else if (calcManager.expressionSet == null) 
                {
                    print("EXPRESSION SET NULL");
                }
                else if (calcManager.expressionSet.GetRange(rangeTitle) == null) 
                {
                    print("RANGE NULL");
                }
                else if (calcManager.expressionSet.GetRange(rangeTitle).Min == null) 
                {
                    print("MIN NULL");
                }

                calcManager.SetOutput(calcManager.expressionSet.GetRange(rangeTitle).Min); 
                break;
            case "Max":
                calcManager.SetOutput(calcManager.expressionSet.GetRange(rangeTitle).Max); 
                break;
            case "GenerateMesh":
                calcManager.toExport = true;
                break;
            case "Save":
                if (Saveable)
                {
                    saveLoadMenu.Save();
                }
                break;
        }
    }

    void Update()
    {
        //TODO: uncomment
        //Saveable = !paramSurface.isGraphing();
    }
}
