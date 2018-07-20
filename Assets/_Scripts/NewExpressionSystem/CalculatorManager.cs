using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class CalculatorManager : MonoBehaviour
{
    [HideInInspector]
    public bool inputReceived;
    public ExpressionSet expressionSet;

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
    public List<ExpressionSet> expressionSetList = new List<ExpressionSet>();

    protected Expressions expressions;
    protected Transform selectedExpr;
    protected JoyStickAggregator joyStickAggregator;

    void Start()
    {
        Initialize();
    }

    protected abstract void Initialize();
    public abstract void deleteVariables(List<string> toDelete);

    protected void addForwarders(Transform obj)
    {
        JoyStickForwarder[] forwarders = obj.GetComponentsInChildren<JoyStickForwarder>();
        foreach (JoyStickForwarder j in forwarders)
        {
            joyStickAggregator.AddForwarder(j);
        }
    }

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
        expressionSetList.Remove(ES);
        inputReceived = true;
    }

    public void ChangeExpressionSet(ExpressionSet ES)
    {
        expressionSet = ES;
        inputReceived = true;
    }

    private string getExpOption()
    {
        title = (expressions.getSelectedBody()) ? expressions.getSelectedBody().getTitle() : "X";
        return title;
    }

    public void ManageFeedback()
    {
        selectedExpr = expressions.getSelectedExpr();
        if (expressions.selectedNotNull())
        {
            feedBack = expressions.getSelectedBody().getFeedBack();
            title = expressions.getSelectedBody().getTitle();
        }

        if (feedBack != null) feedBack.GetComponent<Renderer>().material.color = expressionSet.expValidity[title] ? positiveFeedback : negativeFeedback;
    }

    public void manageText()
    {
        selectedExpr = expressions.getSelectedExpr();
        ExpressionBody exprBody = expressions.getSelectedBody();

        if (selectedExpr == null || exprBody == null) return;

        if (expressions.selectedNotNull())
        {
            textInput = exprBody.getTextInput();
        }

        if (textInput != null)
        {
            int displayLength = (exprBody.isVariable()) ? rangeDisplayLength : expressionDisplayLength;
            textInput.text = displayText(calcInput.currExpression.tokens, calcInput.index, true, displayLength);
        }

        inputReceived = true;
    }

    public bool letterPressed(string buttonID)
    {
        Transform param = expressions.getSelectedExpr();

        //prevents typing of letters when a variable body is selected
        if (expressions.getSelectedBody() && expressions.getSelectedBody().isVariable())
        {
            return false;
        }

        //creates new variable button when new letter pressed
        if (param != null)
        {
            if (expressionSet.hiddenRanges.ContainsKey(buttonID))
            {
                ExpressionSet.getExpressionSet(calcInput.currExpression).ReAddVariable(buttonID);
                param.GetComponent<ParametricExpression>().addVariable(buttonID, null);
            }
            else if (expressionSet.GetRange(buttonID) == null)
            {
                GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
                var.transform.localScale = Vector3.one;
                var.transform.Find("Min").GetComponentInChildren<ExpressionBody>().setExpressionParent(param.transform);
                var.transform.Find("Max").GetComponentInChildren<ExpressionBody>().setExpressionParent(param.transform);
                var.transform.Find("Min").GetComponentInChildren<ExpressionBody>().setPanel(GameObject.Find("ExpressionMenu/ParametrizationPanel").transform);
                var.transform.Find("Max").GetComponentInChildren<ExpressionBody>().setPanel(GameObject.Find("ExpressionMenu/ParametrizationPanel").transform);

                param.GetComponent<ParametricExpression>().addVariable(buttonID, var.transform);
                var.transform.Find("VariableTitle").Find("Body").GetComponent<ExpressionBody>().setTitle(buttonID);
                expressionSet.AddRange(buttonID);
                addForwarders(var.transform);
            }
        }

        return true;
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
