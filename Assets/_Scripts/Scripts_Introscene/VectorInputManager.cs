using UnityEngine;
using System.Collections;
using System.Collections.Generic;

internal class VectorKeyboardInputResponder : FlexMenu.FlexMenuResponder
{
    internal bool isReady = false;
    internal TextMesh xText, yText, zText, currText, tminText, tmaxText;
    //internal List<string> xExpression = new List<string>();
    //internal List<string> yExpression = new List<string>();
    //internal List<string> zExpression = new List<string>();
    //internal List<string> tminExpression = new List<string>();
    //internal List<string> tmaxExpression = new List<string>();

    internal ExpressionSet es;
    internal CalcOutput currExpression;

    internal CustomVectorField.SampleDensity dens;

    //List<string> currExpression = new List<string>();
    int index = 0;
    int maxDisplayLength = 20;

    internal VectorKeyboardInputResponder(TextMesh x, TextMesh y, TextMesh z, TextMesh tmin, TextMesh tmax)
    {
        es = new ExpressionSet();
        xText = x;
        yText = y;
        zText = z;
        tminText = tmin;
        tmaxText = tmax;
        dens = CustomVectorField.SampleDensity.LOW;

        currExpression = es.expressions["X"];
        currText = xText;
    }

    public void initialize()
    {
        es = new ExpressionSet();
        es.expressions["X"].tokens = new List<string>
                {
                    "y"
                };
        es.expressions["Y"].tokens = new List<string>
                {
                    "-x"
                };
        es.expressions["Z"].tokens = new List<string>
                {
                    "0"
                };
        es.ranges["t"].Min.tokens = new List<string>
        {
            "-", "5", "0"
        };
        es.ranges["t"].Max.tokens = new List<string>
        {
            "5", "0"
        };
        currExpression = es.expressions["X"];
        currText = xText;
        index = currExpression.tokens.Count;
        isReady = true;
        xText.text = displayText(es.expressions["X"].tokens, es.expressions["X"].tokens.Count, false);
        yText.text = displayText(es.expressions["Y"].tokens, es.expressions["Y"].tokens.Count, false);
        zText.text = displayText(es.expressions["Z"].tokens, es.expressions["Z"].tokens.Count, false);
        tminText.text = displayText(es.ranges["t"].Min.tokens, es.ranges["t"].Min.tokens.Count, false);
        tmaxText.text = displayText(es.ranges["t"].Max.tokens, es.ranges["t"].Max.tokens.Count, false);
        currText.text = displayText(currExpression.tokens, index, true);
    }

    public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
    {
        switch (sender.name)
        {
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
                isReady = true;
                break;
            default:
                currExpression.tokens.Insert(index, sender.name);
                isReady = true;
                index++;
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
            case "Button_Xinput":
                currExpression = es.expressions["X"];
                currText = xText;
                index = currExpression.tokens.Count;
                break;
            case "Button_Yinput":
                currExpression = es.expressions["Y"];
                currText = yText;
                index = currExpression.tokens.Count;
                break;
            case "Button_Zinput":
                currExpression = es.expressions["Z"];
                currText = zText;
                index = currExpression.tokens.Count;
                break;
            case "tmin":
                currExpression = es.ranges["t"].Min;
                currText = tminText;
                index = currExpression.tokens.Count;
                break;
            case "tmax":
                currExpression = es.ranges["t"].Max;
                currText = tmaxText;
                index = currExpression.tokens.Count;
                break;
            case "high":
                dens = CustomVectorField.SampleDensity.HIGH;
                isReady = true;
                break;
            case "medium":
                dens = CustomVectorField.SampleDensity.MEDIUM;
                isReady = true;
                break;
            case "low":
                dens = CustomVectorField.SampleDensity.LOW;
                isReady = true;
                break;
        }
        //index++;
        xText.text = displayText(es.expressions["X"].tokens, es.expressions["X"].tokens.Count, false);
        yText.text = displayText(es.expressions["Y"].tokens, es.expressions["Y"].tokens.Count, false);
        zText.text = displayText(es.expressions["Z"].tokens, es.expressions["Z"].tokens.Count, false);
        tminText.text = displayText(es.ranges["t"].Min.tokens, es.ranges["t"].Min.tokens.Count, false);
        tmaxText.text = displayText(es.ranges["t"].Max.tokens, es.ranges["t"].Max.tokens.Count, false);
        currText.text = displayText(currExpression.tokens, index, true);

        sender.SetState(1);
    }
    public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider)
    {
        if (sender.State != 2)
            sender.SetState(0);
    }

    public void UpdateText()
    {
        xText.text = displayText(es.expressions["X"].tokens, es.expressions["X"].tokens.Count, false);
        yText.text = displayText(es.expressions["Y"].tokens, es.expressions["Y"].tokens.Count, false);
        zText.text = displayText(es.expressions["Z"].tokens, es.expressions["Z"].tokens.Count, false);
        tminText.text = displayText(es.ranges["t"].Min.tokens, es.ranges["t"].Min.tokens.Count, false);
        tmaxText.text = displayText(es.ranges["t"].Max.tokens, es.ranges["t"].Max.tokens.Count, false);
        currExpression = es.expressions["X"];
        index = currExpression.tokens.Count;
        currText.text = displayText(currExpression.tokens, index, true);
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

public class VectorInputManager : MonoBehaviour
{

    private delegate void InsertOptions(int option);
    int currIndex;

    public FlexMenu keyboard;
    public TextMesh xInputbox, yInputbox, zInputbox, tminInput, tmaxInput;
    VectorKeyboardInputResponder responder;

    public CustomVectorField vectorField;
    public FlowLineParticles flowline;
    public ExpressionSet es;
    public bool overlayUpdate = false;

    // Use this for initialization
    void Start()
    {
        print("START");
        responder = new VectorKeyboardInputResponder(xInputbox, yInputbox, zInputbox, tminInput, tmaxInput);
        keyboard.RegisterResponder(responder);

        responder.initialize();
        print(responder);
    }

    // Update is called once per frame
    void Update()
    {
        if (responder.isReady)
        {
            responder.isReady = false;

            //vectorField.expressionX = compileTokens(responder.xExpression);
            //vectorField.expressionY = compileTokens(responder.yExpression);
            //vectorField.expressionZ = compileTokens(responder.zExpression);
            vectorField.dens = responder.dens;
            es = responder.es;
            vectorField.es = responder.es.ShallowCopy();
            vectorField.UpdateFunctions();

            if (flowline != null)
            {
                flowline.t_min = compileTokens(responder.es.ranges["t"].Min.tokens);
                flowline.t_max = compileTokens(responder.es.ranges["t"].Max.tokens);
                flowline.ForceUpdate();
            }
            overlayUpdate = true;
        }
    }

    public void UpdateEquation()
    {
        print(vectorField);
        print(vectorField.es);
        print(responder);
        print(responder.es);
        vectorField.es = responder.es.ShallowCopy();
        vectorField.UpdateFunctions();
        if (flowline != null)
        {
            flowline.t_min = compileTokens(responder.es.ranges["t"].Min.tokens);
            flowline.t_max = compileTokens(responder.es.ranges["t"].Max.tokens);
            flowline.ForceUpdate();
        }
        responder.UpdateText();
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


