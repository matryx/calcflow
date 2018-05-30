using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class CalculatorManager : MonoBehaviour
{
    [HideInInspector]
    public bool inputReceived;


    protected CalcInput calcInput;

    protected Transform feedBack;
    protected TMPro.TextMeshPro textInput;
    protected string title;

    protected Color positiveFeedback;  //GREEN
    protected Color negativeFeedback = Color.red;

    protected int expressionDisplayLength = 20;
    //TODO: decrease text size to increase range length
    protected int rangeDisplayLength = 3;

    public bool updateOverlay = false;
    public bool toExport = false;

    void Start()
    {
        Initialize();
    }

    protected abstract void Initialize();

    public virtual void SetOutput(CalcOutput output)
    {
        calcInput.ChangeOutput(output, this);
        inputReceived = true;
    }

    public string displayText(List<string> exp, int index0, bool mark, int displayLength)
    {
        string test = "";
        foreach (string s in exp)
        {
            test += s;
        }

        bool end = false;
        bool start = false;
        int forward = 1;
        int back = 0;
        int space = displayLength;
        if (!mark)
        {
            index0 = exp.Count;
        }
        string displayList = (mark) ? "_" : "";
        while (!start || !end)
        {
            if (!end && index0 + forward - 1 < exp.Count)
            {
                string next = exp[index0 + forward - 1];
                next = CleanRepresentation(next);
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
                if (prev == "pi")
                {
                    prev = "π";
                }
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

        return displayList;
    }

    //TODO: Euler's fix
    string CleanRepresentation(string input)
    {
        switch (input)
        {
            case "pi":
                return "π";
            case "arccos":
                return "acos";
            case "arcsin":
                return "asin";
            case "arctan":
                return "atan";
            case "arccosh":
                return "acosh";
            case "arcsinh":
                return "asinh";
            case "arctanh":
                return "atanh";
            default:
                return input;
        }
    }

    public abstract bool letterPressed(string buttonID);

    //handles process of deleting variables
    public abstract void deleteVariables(List<string> toDelete);
}
