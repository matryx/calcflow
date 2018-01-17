using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CalculatorManager : MonoBehaviour
{
    [HideInInspector]
    public ExpressionSet expressionSet;

    [HideInInspector]
    public bool inputReceived;

    public static CalculatorManager _instance;

    CustomParametrizedSurface paramSurface;
    CalcInput calcInput;
    BoundsManager boundsManager;
    PresetMenu presetMenu;
    SaveLoadMenu saveLoadMenu;
    OutputManager outputManager;

    Expressions expressions;
    Transform selectedExpr;
    Transform selectedBody;
    Transform feedBack;
    TMPro.TextMeshPro textInput;
    string title;

    #region constants
    const ExpressionSet.ExpOptions X = ExpressionSet.ExpOptions.X;
    const ExpressionSet.ExpOptions Y = ExpressionSet.ExpOptions.Y;
    const ExpressionSet.ExpOptions Z = ExpressionSet.ExpOptions.Z;
    #endregion

    private Color positiveFeedback;
    private Color negativeFeedback = Color.red;

    int expressionDisplayLength = 20;
    //TODO: decrease text size to increase range length
    int rangeDisplayLength = 3;

    public bool updateOverlay = false;
    internal bool toExport = false;

    void Awake()
    {
        Initialize();
    }

    private void Initialize()
    {
        _instance = this;
        expressions = Expressions._instance;

        paramSurface = CustomParametrizedSurface._instance;
        calcInput = CalcInput._instance;
        boundsManager = BoundsManager._instance;
        outputManager = OutputManager._instance;
        //saveLoadMenu = SaveLoadMenu._instance;
        //presetMenu = PresetMenu._instance;

        if (boundsManager != null) boundsManager.Initialize(this);
        calcInput.Initialize(this);

        calcInput.ChangeOutput(expressionSet.expressions[X]); //need to fix
        if (outputManager != null)
        {
            print("OUTPUT INIIALIZED");
            outputManager.Initialize(this);
        }
        //presetMenu.Initialize(this);
        //saveLoadMenu.Initialize(this);

        ColorUtility.TryParseHtmlString("#64C3A7FF", out positiveFeedback);

        //if (connectedMenus.particleAnimationSettings != null)
        //    connectedMenus.particleAnimationSettings.Initialize(this);
    }

    public void PresetPressed()
    {
        calcInput.ChangeOutput(expressionSet.expressions[X]); //need to fix
        if (boundsManager != null) boundsManager.UpdateButtonText();
        inputReceived = true;
    }

    public void ChangeExpressionSet(ExpressionSet ES)
    {
        expressionSet = ES;
        calcInput.ChangeOutput(expressionSet.expressions[getExpOption()]);
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
        calcInput.ChangeOutput(expressionSet.expressions[X]); //need to fix
        if (boundsManager != null) boundsManager.UpdateButtonText();
        inputReceived = true;
    }

    public void SetOutput(CalcOutput output)
    {
        calcInput.ChangeOutput(output);
    }

    private ExpressionSet.ExpOptions getExpOption()
    {
        ExpressionSet.ExpOptions op = X;
        //title = (expressions.getSelectedBody())? expressions.getSelectedBody().getTitle() : "X";
        title = expressions.getSelectedBody().getTitle();
        print("TITLE: " + title);
        switch (title)
        {
            case "X":
                op = X;
                break;
            case "Y":
                op = Y;
                break;
            case "Z":
                op = Z;
                break;
        }

        return op;
    }

    public void manageText()
    {
        //handle variables too

        selectedExpr = expressions.getSelectedExpr();
        if (selectedExpr == null || expressions.getSelectedBody() == null) return;

        selectedBody = expressions.getSelectedBody().transform;

        if (expressions.selectedNotNull())
        {
            textInput = expressions.getSelectedBody().getTextInput();
            //print("text input: " + textInput);
        }

        if (textInput != null)
        {
            //print("HELLO");
            int displayLength = (expressions.getSelectedBody().isVariable()) ? rangeDisplayLength : expressionDisplayLength;
            textInput.text = displayText(calcInput.currExpression.tokens, calcInput.index, true, displayLength);
        }
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

    public string displayText(List<string> exp, int index0, bool mark, int displayLength)
    {
        string test = "";
        foreach(string s in exp)
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

    void Update()
    {
        if (inputReceived)
        {
            manageText();
            inputReceived = false;
            updateOverlay = true;
            bool isValid = expressionSet.CompileAll();
            ManageFeedback();
            //if (isValid)
            //paramSurface.GenerateParticles();
        }
        if (toExport)
        {
            toExport = false;
            paramSurface.GenerateMesh();
        }
    }

}
