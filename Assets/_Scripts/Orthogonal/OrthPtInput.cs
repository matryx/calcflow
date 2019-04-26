using System.Collections;
using System.Collections.Generic;
using UnityEngine;

//keyboard input 
public class OrthPtInput : MonoBehaviour
{

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        OrthPtInput ptInput;
        internal KeyboardInputResponder(OrthPtInput ptInput)
        {
            this.ptInput = ptInput;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            ptInput.HandleInput(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }

    }

    [HideInInspector]
    public CalcOutput currExpression;
    [HideInInspector]
    public int index = 0;
    [HideInInspector]

    private OrthPtManager orthPtManager;

    private FlexMenu keyboard;

    KeyboardInputResponder responder;

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
                orthPtManager.inputReceived = true;
                break;
            case "Paste":
                /*
                string temp = GUIUtility.systemCopyBuffer;
                List<string> tempList = ExpressionParser.Parse(temp);
                currExpression.tokens.InsertRange(index, tempList);
                index += tempList.Count;
                calcManager.inputReceived = true;
                */
                break;
            #region control_buttons
            case "Button_del":
                if (index > 0)
                {
                    currExpression.tokens.RemoveAt(index - 1);
                    index--;
                    orthPtManager.inputReceived = true;
                }
                break;
            case "Button_Clear":
                index = 0;
                currExpression.tokens.Clear();
                orthPtManager.inputReceived = true;
                break;
            case "Button_Enter":
                orthPtManager.inputReceived = true;
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
        orthPtManager.updateText = true;
    }

    public void Initialize(OrthPtManager pm)
    {
        orthPtManager = pm;
        keyboard = GetComponent<FlexMenu>();
        responder = new KeyboardInputResponder(this);
        keyboard.RegisterResponder(responder);
    }

    public void RewriteInput(float newValue)
    {
        index = 0;
        currExpression.tokens.Clear();
        string s = roundString(newValue, "{0:0.00}");
        if (orthPtManager.eqnInput) s = roundString(newValue, "{0:0.0}");
        foreach (char c in s)
        {
            currExpression.tokens.Insert(index, c.ToString());
            index++;
        }
    }

    public void RewriteInput()
    {
        index = 0;
        currExpression.tokens.Clear();
    }

    public string roundString(float input, string format)
    {
        string a = input.ToString();
        string b = string.Format(format, input);
        if (a.Length <= b.Length)
        {
            return a;
        }
        else
        {
            return b;
        }
    }
}
