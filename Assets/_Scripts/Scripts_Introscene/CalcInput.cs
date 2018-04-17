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
    private Transform letterPanel, variablePanel;
    bool capitalized = false;

    KeyboardInputResponder responder;
    Expressions expressions;
    Scroll paramScroll;
    VariableShortcut variableShortcut;
    JoyStickAggregator joyStickAggregator;
    Color errorColor;
    Color selectedColor;
    GameObject errorPopup;
    List<string> varsToDelete = new List<string>();

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
        variableShortcut = VariableShortcut._instance;
        letterPanel = transform.Find("LetterPanel");
        variablePanel = transform.Find("VariableShortcut");
        paramScroll = GameObject.Find("PanelBodyParam").transform.GetComponent<Scroll>();
        joyStickAggregator = paramScroll.GetComponent<JoyStickAggregator>();
        errorColor = Color.red;
        ColorUtility.TryParseHtmlString("#4072ABFF", out selectedColor);
        errorPopup = Instantiate(Resources.Load("Popups/VariableError")) as GameObject;
        errorPopup.SetActive(false);
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

    public void disablePopup()
    {
        StartCoroutine(ScaleTo(errorPopup.transform, Vector3.one, Vector3.zero, 0.1f));
    }

    IEnumerator ScaleTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            obj.localScale = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        obj.localScale = end;
        if (end == Vector3.zero) obj.gameObject.SetActive(false);
    }

    //called when button on keyboard pressed
    public void HandleInput(string buttonID)
    {
        if (currExpression == null) return;

        #region switch
        switch (buttonID)
        {
            default:
                Transform param = expressions.getSelectedExpr();
                letterPanel.GetComponent<KeyboardFlexPanel>().ChangeSelectedColor(selectedColor);
                variablePanel.GetComponent<KeyboardFlexPanel>().ChangeSelectedColor(selectedColor);

                //if typing a single letter
                if (buttonID.Length == 1 && buttonID[0] > 96 && buttonID[0] < 123)
                {
                    //prevents typing of letters when a variable body is selected
                    if (expressions.getSelectedBody().isVariable())
                    {
                        errorPopup.SetActive(true);
                        errorPopup.transform.position = transform.position + new Vector3(0, -1.5f, -1);
                        StartCoroutine(ScaleTo(errorPopup.transform, Vector3.zero, Vector3.one, 0.1f));
                        letterPanel.GetComponent<KeyboardFlexPanel>().ChangeSelectedColor(errorColor);
                        variablePanel.GetComponent<KeyboardFlexPanel>().ChangeSelectedColor(errorColor);
                        break;
                    }

                    if (variableShortcut == null) variableShortcut = VariableShortcut._instance;
                    variableShortcut.recordVarPress(buttonID);

                    //creates new variable button when new letter pressed
                    if (param != null) 
                    {
                        if (calcManager.expressionSet.hiddenRanges.ContainsKey(buttonID))
                        {
                            currExpression.expSet.ReAddVariable(buttonID);
                            param.GetComponent<ParametricExpression>().addVariable(buttonID, null);
                        }
                        else if (!calcManager.expressionSet.ranges.ContainsKey(buttonID))
                        {
                            GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
                            var.GetComponent<ExpressionComponent>().setExpressionParent(param);
                            var.GetComponent<ExpressionComponent>().setPanel(transform.parent.Find("ParametrizationPanel"));
                            param.GetComponent<ParametricExpression>().addVariable(buttonID, var.transform);
                            var.transform.Find("VariableTitle").Find("Body").GetComponent<ExpressionBody>().setTitle(buttonID);
                            calcManager.expressionSet.AddRange(buttonID);
                            addForwarders(var.transform);
                        }

                    }
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
                    string toDelete = currExpression.tokens[index - 1];
                    currExpression.tokens.RemoveAt(index - 1);
                    index--;

                    if (currExpression.expSet == null) break;
                    if (currExpression.expSet.GetTotalOccurence(toDelete) == 0)
                    {
                        calcManager.expressionSet.RemoveVariable(toDelete);
                        expressions.getSelectedExpr().GetComponent<ParametricExpression>().deleteVariable(toDelete);
                    }
                }

                break;
            case "Button_Clear":
                index = 0;
                List<string> toDel = currExpression.ClearTokens();
                if (toDel == null) break;

                foreach (string del in toDel)
                {
                    calcManager.expressionSet.RemoveVariable(del);
                    expressions.getSelectedExpr().GetComponent<ParametricExpression>().deleteVariable(del);
                }

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


