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

    public static CalcInput _instance;

    CalculatorManager calcManager;
    VariableShortcut variableShortcut;
    Expressions expressions;

    FlexMenu keyboard;
    KeyboardInputResponder responder;

    List<string> varsToDelete = new List<string>();

    Transform letterPanel, variablePanel;
    GameObject errorPopup;
    Color errorColor, selectedColor;

    string variableErrorMessage = "Typing letters in variables is not allowed.";
    string vectorFieldErrorMessage = "Typing non-xyz letters in vector field expressions is not allowed.";
    bool capitalized = false;

    void Awake()
    {
        _instance = this;
    }

    public void Initialize(CalculatorManager cm)
    {
        calcManager = cm;
        expressions = Expressions._instance;

        keyboard = GetComponent<FlexMenu>();
        responder = new KeyboardInputResponder(this);
        keyboard.RegisterResponder(responder);

        letterPanel = transform.Find("LetterPanel");
        variablePanel = transform.Find("VariableShortcut");
        variableShortcut = VariableShortcut._instance;

        errorColor = Color.red;
        ColorUtility.TryParseHtmlString("#4072ABFF", out selectedColor);
        errorPopup = Instantiate(Resources.Load("Popups/VariableError")) as GameObject;
        errorPopup.SetActive(false);
    }

    //called by CalculatorManager
    public void ChangeOutput(CalcOutput calcOutput, CalculatorManager cm)
    {
        this.calcManager = cm;
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

    bool checkForError(string buttonID)
    {
        //prevents typing of letters when a variable body is selected
        if (!expressions.GetSelectedBody())
        {
            return true;
        }
        else
        {
            if (expressions.GetSelectedBody().IsVariable())
            {
                showError(variableErrorMessage);
                return true;
            }
        }

        List<string> vectorFieldVars = new List<string> { "x", "y", "z" };

        if (calcManager == VecFieldManager._instance)
        {
            if (!vectorFieldVars.Contains(buttonID))
            {
                showError(vectorFieldErrorMessage);
                return true;
            }
        }

        return false;
    }

    void showError(string message)
    {
        letterPanel.GetComponent<KeyboardFlexPanel>().ChangeSelectedColor(errorColor);
        variablePanel.GetComponent<KeyboardFlexPanel>().ChangeSelectedColor(errorColor);

        if (!errorPopup.activeSelf)
        {
            errorPopup.SetActive(true);
            errorPopup.transform.GetComponentInChildren<TMPro.TextMeshPro>().text = message;
            errorPopup.transform.SetParent(transform);
            errorPopup.transform.localPosition = new Vector3(0, -1.5f, -0.5f);
            StartCoroutine(ScaleTo(errorPopup.transform, Vector3.zero, Vector3.one, 0.1f));
        }
    }

    //called when button on keyboard pressed
    public void HandleInput(string buttonID)
    {
        if (currExpression == null) return;
        bool error = false;

        #region switch
        switch (buttonID)
        {
            default:
                letterPanel.GetComponent<KeyboardFlexPanel>().ChangeSelectedColor(selectedColor);
                variablePanel.GetComponent<KeyboardFlexPanel>().ChangeSelectedColor(selectedColor);

                //if typing a single letter
                if (buttonID.Length == 1 && buttonID[0] > 96 && buttonID[0] < 123)
                {
                    error = checkForError(buttonID);

                    //BUG: shouldn't create variable shortcut when nothing selected
                    if (!error)
                    {
                        calcManager.LetterPressed(buttonID);

                        if (variableShortcut == null) variableShortcut = VariableShortcut._instance;
                        variableShortcut.recordVarPress(buttonID);
                    }
                }

                if (!error)
                {
                    currExpression.tokens.Insert(index, buttonID);
                    index++;
                }

                break;
            case "Paste":
                string temp = GUIUtility.systemCopyBuffer;
                List<string> tempList = ExpressionParser.Parse(temp);
                currExpression.tokens.InsertRange(index, tempList);
                index += tempList.Count;
                break;
            #region control_buttons
            case "Button_del":
                List<string> toDelete = new List<string>();

                if (index > 0)
                {
                    string s = currExpression.tokens[index - 1];
                    toDelete.Add(s);
                    currExpression.tokens.RemoveAt(index - 1);
                    index--;

                    if (ExpressionSet.getExpressionSet(currExpression) == null) break;
                    if (ExpressionSet.getExpressionSet(currExpression).GetTotalOccurence(s) == 0)
                    {
                        calcManager.DeleteVariables(toDelete);
                    }
                }

                break;
            case "Button_Clear":
                index = 0;
                List<string> toDel = currExpression.ClearTokens();
                if (toDel == null) break;

                calcManager.DeleteVariables(toDel);
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

    void toggleCapital()
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


