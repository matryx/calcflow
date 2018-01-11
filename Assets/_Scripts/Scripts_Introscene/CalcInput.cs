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

    private CalculatorManager calcManager;
    public static CalcInput _instance;

    private FlexMenu keyboard;
    private Transform letterPanel;
    bool capitalized = false;

    KeyboardInputResponder responder;
    Expressions expressions;
    Scroll paramScroll;
    JoyStickAggregator joyStickAggregator;

    ExpressionSet.ExpOptions X = ExpressionSet.ExpOptions.X;
    ExpressionSet.ExpOptions Y = ExpressionSet.ExpOptions.Y;
    ExpressionSet.ExpOptions Z = ExpressionSet.ExpOptions.Z;

    List<string> nonLetters = new List<string> { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "(", ")", "^", "/", "*", "-", "+", "." };

    private void Awake()
    {
        _instance = this;
    }

    public void Initialize(CalculatorManager cm)
    {
        calcManager = cm;
        keyboard = GetComponent<FlexMenu>();
        responder = new KeyboardInputResponder(this);
        keyboard.RegisterResponder(responder);
        expressions = Expressions._instance;
        letterPanel = transform.Find("LetterPanel");
        paramScroll = GameObject.Find("PanelBodyParam").transform.GetComponent<Scroll>();
        joyStickAggregator = paramScroll.GetComponent<JoyStickAggregator>();
    }

    private void addForwarders(Transform obj)
    {
        JoyStickForwarder[] forwarders = obj.GetComponentsInChildren<JoyStickForwarder>();
        foreach (JoyStickForwarder j in forwarders)
        {
            joyStickAggregator.AddForwarder(j);
        }
    }

    //called by CalculatorManager
    public void ChangeOutput(CalcOutput calcOutput)
    {
        currExpression = calcOutput;
        index = (currExpression == null) ?
                0 : currExpression.tokens.Count;
    }

    //called when button on keyboard pressed
    public void HandleInput(string buttonID)
    {
        if (currExpression == null) return;

        #region switch
        switch (buttonID)
        {
            default:
                //TODO: add forwarder just like it's done in ExpressionSelector
                //      - need access to param scroll

                //creates new variable when new letter is pressed
                if (!nonLetters.Contains(buttonID) && !calcManager.expressionSet.ranges.ContainsKey(buttonID))
                {
                    Transform param = expressions.getSelectedExpr();
                    GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
                    var.GetComponent<ExpressionComponent>().setExpressionParent(param);
                    var.GetComponent<ExpressionComponent>().setPanel(transform.parent.Find("ParametrizationPanel"));
                    param.GetComponent<ParametricExpression>().addVariable(var.transform);
                    var.transform.Find("VariableTitle").Find("Body").GetComponent<ExpressionBody>().setTitle(buttonID);
                    calcManager.expressionSet.AddRange(buttonID);
                    addForwarders(var.transform);
                }
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

    private void toggleCapital()
    {
        foreach (Transform child in letterPanel)
        {
            if (child.name != "ToggleCaps")
            {
                child.name = (capitalized) ? child.name.ToLower() : child.name.ToUpper();

                child.GetComponentInChildren<TMPro.TextMeshPro>().text = (capitalized) ?
                                                                 child.GetComponentInChildren<TMPro.TextMeshPro>().text.ToLower() :
                                                                 child.GetComponentInChildren<TMPro.TextMeshPro>().text.ToUpper();
            }
            else
            {
                child.GetComponentInChildren<TMPro.TextMeshPro>().text = (capitalized) ?
                                                                 child.GetComponentInChildren<TMPro.TextMeshPro>().text.ToUpper() :
                                                                 child.GetComponentInChildren<TMPro.TextMeshPro>().text.ToLower();
            }
        }

        capitalized = !capitalized;
    }
}


