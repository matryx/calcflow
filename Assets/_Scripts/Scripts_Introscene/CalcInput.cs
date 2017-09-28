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

    private CalcManager calcManager;

    private FlexMenu keyboard;

    KeyboardInputResponder responder;

    ExpressionSet.ExpOptions X = ExpressionSet.ExpOptions.X;
    ExpressionSet.ExpOptions Y = ExpressionSet.ExpOptions.Y;
    ExpressionSet.ExpOptions Z = ExpressionSet.ExpOptions.Z;

    public void ChangeOutput(CalcOutput calcOutput) 
    {
        currExpression = calcOutput;
        index = currExpression.tokens.Count;
    }

    public void HandleInput(string buttonID)
    {
        print(buttonID + " fired");
        #region switch
        switch (buttonID)
        {
            default:
                currExpression.tokens.Insert(index, buttonID);
                index++;
                calcManager.inputReceived = true;
                break;
            case "Paste":
                string temp = GUIUtility.systemCopyBuffer;
                List<string> tempList = ExpressionParser.Parse(temp);
                currExpression.tokens.InsertRange(index, tempList);
                index += tempList.Count;
                calcManager.inputReceived = true;
                break;
            #region control_buttons
            case "Button_del":
                if (index > 0)
                {
                    currExpression.tokens.RemoveAt(index - 1);
                    index--;
                    calcManager.inputReceived = true;
                }
                break;
            case "Button_Clear":
                index = 0;
                currExpression.tokens.Clear();
                calcManager.inputReceived = true;
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
            #endregion
        }
        #endregion

    }

    public void Initialize(CalcManager cm)
    {
        calcManager = cm;
        keyboard = GetComponent<FlexMenu>();
        responder = new KeyboardInputResponder(this);
        keyboard.RegisterResponder(responder);
    }

}


