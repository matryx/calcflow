using UnityEngine;
using System.Collections;
using System.Collections.Generic;

internal class DoubleIntegralInputResponder : FlexMenu.FlexMenuResponder
{
    internal bool isReady = false;
    internal TextMesh zText,
        uMinText, uMaxText, vMinText, vMaxText,
        currText;
    //internal List<string> xExpression = new List<string>();
    //internal List<string> yExpression = new List<string>();
    internal List<string> zExpression = new List<string>();
    internal List<string> uMinExp = new List<string>();
    internal List<string> uMaxExp = new List<string>();
    internal List<string> vMinExp = new List<string>();
    internal List<string> vMaxExp = new List<string>();

    List<string> currExpression = new List<string>();
    int index = 0;
    int maxDisplayLength = 20;

    internal DoubleIntegralInputResponder(TextMesh z, TextMesh umin, TextMesh umax, TextMesh vmin, TextMesh vmax)
    {
        //xText = x;
        //yText = y;
        zText = z;
        uMinText = umin;
        uMaxText = umax;
        vMinText = vmin;
        vMaxText = vmax;

        currExpression = zExpression;
        currText = zText;
    }

    public void initialize()
    {
        zExpression = new List<string>
                {
                    "cos(", "x", ")", "+", "cos(", "y", ")", "+", "3"
                };
        uMinExp = new List<string>
        {
            "-","5"
        };
        uMaxExp = new List<string> {
            "5"
        };
        vMinExp = new List<string>
        {
            "-","5"
        };
        vMaxExp = new List<string>
        {
            "5"
        };
        currExpression = zExpression;
        currText = zText;
        index = currExpression.Count;
        isReady = true;
        //xText.text = displayText(xExpression, xExpression.Count, false);
        //yText.text = displayText(yExpression, yExpression.Count, false);
        zText.text = displayText(zExpression, zExpression.Count, false);
        uMinText.text = displayText(uMinExp, uMinExp.Count, false);
        uMaxText.text = displayText(uMaxExp, uMaxExp.Count, false);
        vMinText.text = displayText(vMinExp, vMinExp.Count, false);
        vMaxText.text = displayText(vMaxExp, vMaxExp.Count, false);
        currText.text = displayText(currExpression, index, true);
    }

    public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
    {
        switch (sender.name)
        {
            case "Button_del":
                if (index > 0)
                {
                    currExpression.RemoveAt(index - 1);
                    isReady = true;
                    index--;
                }
                break;
            case "Button_Clear":
                index = 0;
                currExpression.Clear();
                break;
            case "Button_Enter":
                isReady = true;
                break;
            default:
                currExpression.Insert(index, sender.name);
                isReady = true;
                index++;
                break;
            case "Button_left":
                index--;
                if (index < 0) index = 0;
                break;
            case "Button_right":
                index++;
                if (index > currExpression.Count) index = currExpression.Count;
                break;
            case "Button_start":
                index = 0;
                break;
            case "Button_end":
                index = currExpression.Count;
                break;
            //case "Button_Xinput":
            //    currExpression = xExpression;
            //    currText = xText;
            //    index = xExpression.Count;
            //    break;
            //case "Button_Yinput":
            //    currExpression = yExpression;
            //    currText = yText;
            //    index = yExpression.Count;
            //    break;
            case "Button_Zinput":
                currExpression = zExpression;
                currText = zText;
                index = zExpression.Count;
                break;
            case "umin":
                currExpression = uMinExp;
                currText = uMinText;
                index = uMinExp.Count;
                break;
            case "umax":
                currExpression = uMaxExp;
                currText = uMaxText;
                index = uMaxExp.Count;
                break;
            case "vmin":
                currExpression = vMinExp;
                currText = vMinText;
                index = vMinExp.Count;
                break;
            case "vmax":
                currExpression = vMaxExp;
                currText = vMaxText;
                index = vMaxExp.Count;
                break;
        }
        //index++;
        //xText.text = displayText(xExpression, xExpression.Count, false);
        //yText.text = displayText(yExpression, yExpression.Count, false);
        zText.text = displayText(zExpression, zExpression.Count, false);
        uMinText.text = displayText(uMinExp, uMinExp.Count, false);
        uMaxText.text = displayText(uMaxExp, uMaxExp.Count, false);
        vMinText.text = displayText(vMinExp, vMinExp.Count, false);
        vMaxText.text = displayText(vMaxExp, vMaxExp.Count, false);
        currText.text = displayText(currExpression, index, true);

        sender.SetState(1);
    }
    public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider)
    {
        if (sender.State != 2)
            sender.SetState(0);
    }

    public string displayText(List<string> exp, int index0, bool mark)
    {
        bool end = false;
        bool start = false;
        int forward = 1;
        int back = 0;
        int space = maxDisplayLength;
        string displayList = (mark) ? "_" : "";
        while (!start || !end)
        {
            if (!end && index0 + forward - 1 < exp.Count)
            {
                string next = exp[index0 + forward - 1];
                if (space - next.Length > 0)
                {
                    displayList += (next);
                    space -= next.Length;
                    forward++;
                }
                else
                {
                    displayList += "...";
                    end = true;
                }
            }
            else end = true;
            if (!start && index0 - back > 0)
            {
                string prev = exp[index0 - back - 1];
                if (space - prev.Length > 0)
                {
                    displayList = prev + displayList;
                    space -= prev.Length;
                    back++;
                }
                else
                {
                    displayList = "..." + displayList;
                    start = true;
                }
            }
            else start = true;
        }
        //currText.text = displayList;
        return displayList;
    }
}

public class DoubleIntegralInput : MonoBehaviour
{

    private delegate void InsertOptions(int option);
    int currIndex;

    public FlexMenu keyboard;
    public TextMesh zInputbox, uMinInput, uMaxInput, vMinInput, vMaxInput;
    DoubleIntegralInputResponder responder;

    //public CustomVectorField vectorField;
    public HeightMapIntegration integral;

    // Use this for initialization
    void Start()
    {
        responder = new DoubleIntegralInputResponder(zInputbox, uMinInput, uMaxInput, vMinInput, vMaxInput);
        keyboard.RegisterResponder(responder);

        responder.initialize();
    }

    // Update is called once per frame
    void Update()
    {
        if (responder.isReady)
        {
            responder.isReady = false;

            integral.expressionZ = compileTokens(responder.zExpression);
            integral.u_min = compileTokens(responder.uMinExp);
            integral.u_max = compileTokens(responder.uMaxExp);
            integral.v_min = compileTokens(responder.vMinExp);
            integral.v_max = compileTokens(responder.vMaxExp);
            integral.UpdateEquation();
        }
        //integral.expressionZ = compileTokens(responder.zExpression);
        //integral.u_min = compileTokens(responder.uMinExp);
        //integral.u_max = compileTokens(responder.uMaxExp);
        //integral.v_min = compileTokens(responder.vMinExp);
        //integral.v_max = compileTokens(responder.vMaxExp);
    }

    public string compileTokens(List<string> equation)
    {
        List<string> eq = new List<string>(equation);
        int paren = 0;
        if (eq.Count == 0)
        {
            return "0";
        }
        for (int i = 0; i < eq.Count; i++)
        {
            string curr = eq[i];
            /* counting parens */
            if (isCloseP(curr)) paren--;
            if (hasOpenP(curr)) paren++;

            if (i == 0) continue;

            string last = eq[i - 1];

            /* Situations where we must add multiplication symbols.*/
            if ((isNum(last) && !isSymbol(curr) && !isNum(curr) && !isCloseP(curr))
             || (isVar(last) && !isSymbol(curr) && !isCloseP(curr))
             || (isCloseP(last) && !isSymbol(curr) && !isCloseP(curr)))
            {
                eq.Insert(i++, "*");
            }


        }
        while (paren < 0)
        {
            eq.Insert(0, "(");
            paren++;
        }
        while (paren > 0)
        {
            eq.Add(")");
            paren--;
        }
        return string.Join("", eq.ToArray());
    }

    #region helper Functions
    /// <summary>
    /// checks if string represents a variable.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    // Heuristic: checks if the last char is a lowercase letter. 
    //            This way we get 'pi' but not 'cos('
    bool isVar(string s)
    {
        if (s[s.Length - 1] > 96 && s[s.Length - 1] < 123) return true;
        return false;
    }
    /// <summary>
    /// checks if string represents a number.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    // Heuristic: checks if the last char is a number. 
    bool isNum(string s)
    {
        if (s[s.Length - 1] == '.') return true;
        if (s[s.Length - 1] > 47 && s[s.Length - 1] < 58) return true;
        return false;
    }
    /// <summary>
    /// checks if string represents a close paren.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    bool isCloseP(string s)
    {
        if (s[s.Length - 1] == ')') return true;
        return false;
    }
    /// <summary>
    /// Checks if last char is an open paren. Captures functions and open parens.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    bool hasOpenP(string s)
    {
        if (s[s.Length - 1] == '(') return true;
        return false;
    }

    /// <summary>
    /// checks if string represents a mathematical operator.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    bool isSymbol(string s)
    {
        if (s[s.Length - 1] == '^') return true;
        if (s[s.Length - 1] > 41 && s[s.Length - 1] < 48) return true;
        return false;
    }
    #endregion

}


