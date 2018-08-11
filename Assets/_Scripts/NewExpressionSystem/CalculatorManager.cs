using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class CalculatorManager : MonoBehaviour
{
    [HideInInspector]
    public bool inputReceived;

    public ExpressionSet expressionSet;
    public List<ExpressionSet> expressionSetList = new List<ExpressionSet>();
    public bool updateOverlay = false;
    public bool toExport = false;

    protected CalcInput calcInput;
    protected JoyStickAggregator joyStickAggregator;

    protected PresetMenu presetMenu;
    protected SaveLoadMenu saveLoadMenu;

    protected Expressions expressions;

    protected Transform selectedExpr;
    protected Transform feedBack;

    protected TMPro.TextMeshPro textInput;
    protected string title;

    protected Color positiveFeedback;  
    protected Color negativeFeedback = Color.red;

    protected int expressionDisplayLength = 20;
    protected int rangeDisplayLength = 4;

    void Start()
    {
        Initialize();
    }

    protected abstract void Initialize();
    public abstract void DeleteVariables(List<string> toDelete);

    protected void addForwarders(Transform obj)
    {
        JoyStickForwarder[] forwarders = obj.GetComponentsInChildren<JoyStickForwarder>();
        foreach (JoyStickForwarder j in forwarders)
        {
            joyStickAggregator.AddForwarder(j);
        }
    }

    //TODO: make graph update when data changes

    public virtual void SetOutput(CalcOutput output)
    {
        calcInput.ChangeOutput(output, this);
        inputReceived = true;
    }

    public void AddExpressionSet(ExpressionSet ES)
    {
        expressionSetList.Add(ES);
        inputReceived = true;
    }

    public void RemoveExpressionSet(ExpressionSet ES)
    {
        if (ES == expressionSet) expressionSet = null;
        expressionSetList.Remove(ES);
        inputReceived = true;
    }

    public void ChangeExpressionSet(ExpressionSet ES)
    {
        expressionSet = ES;
        inputReceived = true;
    }

    public void ManageFeedback()
    {
        selectedExpr = expressions.GetSelectedExpr();
        if (expressions.SelectedNotNull())
        {
            feedBack = expressions.GetSelectedBody().GetFeedBack();
            title = expressions.GetSelectedBody().GetTitle();
        }

        if (feedBack != null) feedBack.GetComponent<Renderer>().material.color = expressionSet.expValidity[title] ? positiveFeedback : negativeFeedback;
    }

    public void ManageText()
    {
        selectedExpr = expressions.GetSelectedExpr();
        ExpressionBody exprBody = expressions.GetSelectedBody();

        if (selectedExpr == null || exprBody == null) return;

        if (expressions.SelectedNotNull())
        {
            textInput = exprBody.GetTextInput();
        }

        if (textInput != null)
        {
            int displayLength = (exprBody.IsVariable()) ? rangeDisplayLength : expressionDisplayLength;

            if (calcInput.currExpression == null)
            {
                textInput.text = "";
            }
            else
            {
                textInput.text = DisplayText(calcInput.currExpression.tokens, calcInput.index, true, displayLength);
            }
        }
    }

    public void LetterPressed(string buttonID)
    {
        Transform param = expressions.GetSelectedExpr();

        //creates new variable button when new letter pressed
        if (param != null && ParametricManager._instance == this)
        {
            if (expressionSet.hiddenRanges.ContainsKey(buttonID))
            {
                ExpressionSet.getExpressionSet(calcInput.currExpression).ReAddVariable(buttonID);
                param.GetComponent<ParametricExpression>().AddVariable(buttonID, null);
            }
            else if (expressionSet.GetRange(buttonID) == null)
            {
                AddNewParametricVariable(buttonID, param);
            }
        }
    }

    void AddNewParametricVariable (string buttonID, Transform param)
    {
        GameObject var = CreateVariable(param);

        param.GetComponent<ParametricExpression>().AddVariable(buttonID, var.transform);
        var.transform.Find("VariableTitle").Find("Body").GetComponent<ExpressionBody>().SetTitle(buttonID);
        expressionSet.AddRange(buttonID);
        addForwarders(var.transform);
    }

    GameObject CreateVariable(Transform param)
    {
        GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
        var.transform.localScale = Vector3.one;
        var.transform.Find("Min").GetComponentInChildren<ExpressionBody>().SetManager(ParametricManager._instance);
        var.transform.Find("Max").GetComponentInChildren<ExpressionBody>().SetManager(ParametricManager._instance);
        var.transform.Find("Min").GetComponentInChildren<ExpressionBody>().SetExpressionParent(param.transform);
        var.transform.Find("Max").GetComponentInChildren<ExpressionBody>().SetExpressionParent(param.transform);

        return var;
    }

    string DisplayText(List<string> exp, int index0, bool mark, int displayLength)
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
}
