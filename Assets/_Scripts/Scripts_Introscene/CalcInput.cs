using UnityEngine;
using System.Collections;
using System.Collections.Generic;


public class CalcInput : MonoBehaviour
{
    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        CalcInput calcInput;
        internal KeyboardInputResponder(CalcInput calcInput)
        {
            this.calcInput = calcInput;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            calcInput.HandleInput(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }

    }


    [HideInInspector]
    public CalcOutput currExpression;
    [HideInInspector]
    public int index = 0;
    [HideInInspector]

    //private CalcManager calcManager;
    private CalculatorManager calcManager;
    public static CalcInput _instance;

    private FlexMenu keyboard;
    private Transform letterPanel;
    bool capitalized = false;

    KeyboardInputResponder responder;

    ExpressionSet.ExpOptions X = ExpressionSet.ExpOptions.X;
    ExpressionSet.ExpOptions Y = ExpressionSet.ExpOptions.Y;
    ExpressionSet.ExpOptions Z = ExpressionSet.ExpOptions.Z;

    private void Awake()
    {
        _instance = this;
    }

    //public void Initialize(CalcManager cm)
    public void Initialize(CalculatorManager cm)
    {
        calcManager = cm;
        keyboard = GetComponent<FlexMenu>();
        responder = new KeyboardInputResponder(this);
        keyboard.RegisterResponder(responder);
        letterPanel = transform.Find("LetterPanel");
    }

    //called by CalcManager
    public void ChangeOutput(CalcOutput calcOutput)
    {
        currExpression = calcOutput;
        index = currExpression.tokens.Count;
    }

    //called when button on keyboard pressed
    public void HandleInput(string buttonID)
    {
        #region switch
        switch (buttonID)
        {
            default:
                currExpression.tokens.Insert(index, buttonID);
                index++;
                break;
            case "Paste":
                string temp = GUIUtility.systemCopyBuffer;
                List<string> tempList = ExpressionParser.Parse(temp);
                currExpression.tokens.InsertRange(index, tempList);
                index += tempList.Count;
                break;
            #region control_buttons
            case "Button_del":
                if (index > 0)
                {
                    currExpression.tokens.RemoveAt(index - 1);
                    index--;
                }
                break;
            case "Button_Clear":
                index = 0;
                currExpression.tokens.Clear();
                break;
            case "Button_Enter":
                calcManager.inputReceived = true;
                break;
            case "Button_left":
                index--;
                if (index < 0) index = 0;
                break;
            case "Button_right":
                index++;
                if (index > currExpression.tokens.Count) index = currExpression.tokens.Count;
                break;
            case "Button_start":
                index = 0;
                break;
            case "Button_end":
                index = currExpression.tokens.Count;
                break;
            case "ToggleCaps":
                toggleCapital();
                break;
                #endregion
        }
        #endregion

        calcManager.inputReceived = true;
    }

    //NOTE: tested and works but calcinput is disconnected 
    private void toggleCapital()
    {
        foreach (Transform child in letterPanel)
        {
            if (child.name != "ToggleCaps")
            {
                child.name = (capitalized) ? child.name.ToLower() : child.name.ToUpper();

                child.GetComponentInChildren<TextMesh>().text = (capitalized) ?
                                                                 child.GetComponentInChildren<TextMesh>().text.ToLower() :
                                                                 child.GetComponentInChildren<TextMesh>().text.ToUpper();
            }
            else
            {
                child.GetComponentInChildren<TextMesh>().text = (capitalized) ?
                                                                 child.GetComponentInChildren<TextMesh>().text.ToUpper() :
                                                                 child.GetComponentInChildren<TextMesh>().text.ToLower();
            }
        }

        capitalized = !capitalized;
    }
}


