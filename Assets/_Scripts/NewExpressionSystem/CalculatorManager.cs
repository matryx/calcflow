using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CalculatorManager : MonoBehaviour
{
    [HideInInspector]
    public ExpressionSet expressionSet;

    [HideInInspector]
    public bool inputReceived;

    CustomParametrizedSurface paramSurface;
    CalcInput calcInput;
    BoundsManager boundsManager;
    PresetMenu presetMenu;
    SaveLoadMenu saveLoadMenu;

    Expressions expressions;
    Transform selectedExpr;
    Transform feedBack;
    TMPro.TextMeshPro textMeshPro;

    #region constants
    const ExpressionSet.ExpOptions X = ExpressionSet.ExpOptions.X;
    const ExpressionSet.ExpOptions Y = ExpressionSet.ExpOptions.Y;
    const ExpressionSet.ExpOptions Z = ExpressionSet.ExpOptions.Z;
    const ExpressionSet.RangeOptions u = ExpressionSet.RangeOptions.u;
    const ExpressionSet.RangeOptions v = ExpressionSet.RangeOptions.v;
    #endregion

    private Color positiveFeedback = new Color(0, 204, 54);
    private Color negativeFeedback = Color.red;

    int maxDisplayLength = 20;
    int rangeDisplayLength = 5;

    public bool updateOverlay = false;
    internal bool toExport = false;

    void Awake()
    {
        Initialize();
    }

    private void Initialize()
    {
        expressions = Expressions._instance;

        paramSurface = CustomParametrizedSurface._instance;
        calcInput = CalcInput._instance;
        boundsManager = BoundsManager._instance;
        saveLoadMenu = SaveLoadMenu._instance;
        presetMenu = PresetMenu._instance;

        if (boundsManager != null) boundsManager.Initialize(this);
        calcInput.Initialize(this);

        calcInput.ChangeOutput(expressionSet.expressions[X]);
        presetMenu.Initialize(this);

        saveLoadMenu.Initialize(this);

        //if (connectedMenus.particleAnimationSettings != null)
        //    connectedMenus.particleAnimationSettings.Initialize(this);
    }

    public void PresetPressed()
    {
        calcInput.ChangeOutput(expressionSet.expressions[X]);
        if (boundsManager != null) boundsManager.UpdateButtonText();
        inputReceived = true;
    }

    public void ChangeExpressionSet(ExpressionSet ES)
    {
        expressionSet = ES;
        calcInput.ChangeOutput(expressionSet.expressions[X]);
        manageText();
        if (boundsManager != null) boundsManager.UpdateButtonText();
    }

    public void LoadSavedExpressionSets(List<ExpressionSet> expressionSets)
    {
        List<ExpressionSet> ess = new List<ExpressionSet>();
        for (int i = 0; i < expressionSets.Count; i++)
        {
            ess.Add(expressionSets[i].DeepCopy());
            ess[ess.Count - 1].CompileAll();
        }
        paramSurface.expressionSets = ess;
        expressionSet = paramSurface.expressionSets[0];
        calcInput.ChangeOutput(expressionSet.expressions[X]);
        if (boundsManager != null) boundsManager.UpdateButtonText();
        inputReceived = true;
    }

    public void SetOutput(CalcOutput output)
    {
        calcInput.ChangeOutput(output);
    }

    public void manageText()
    {
        //handle variables too
        textMeshPro = expressions.getSelectedBody().getTextMeshPro();
        if (textMeshPro != null)
        {
            textMeshPro.text = displayText(expressionSet.expressions[X].tokens, calcInput.index, calcInput.currExpression == expressionSet.expressions[X], maxDisplayLength);
        }
    }

    public void ManageFeedback()
    {
        selectedExpr = expressions.getSelectedExpr();
        feedBack = expressions.getSelectedBody().getFeedBack();
        if (feedBack != null) feedBack.GetComponent<Renderer>().material.color = expressionSet.expValidity["X"] ? positiveFeedback : negativeFeedback;
    }

    public string displayText(List<string> exp, int index0, bool mark, int displayLength)
    {
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
        //currText.text = displayList;
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

    void Update()
    {
        if (inputReceived)
        {
            manageText();
            inputReceived = false;
            updateOverlay = true;
            bool isValid = expressionSet.CompileAll();
            ManageFeedback();
            if (isValid)
                paramSurface.GenerateParticles();
        }
        if (toExport)
        {
            toExport = false;
            paramSurface.GenerateMesh();
        }
    }

}
