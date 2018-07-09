using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VecFieldManager : CalculatorManager
{
    //[HideInInspector]
    //public bool inputReceived;

    public static VecFieldManager _instance;
    JoyStickAggregator joyStickAggregator;
    Scroll paramScroll;
    CustomVectorField vecField;
    //BoundsManager boundsManager;
    PresetMenu presetMenu;
    SaveLoadMenu saveLoadMenu;
    OutputManager outputManager;

    Expressions expressions;
    Transform selectedExpr;
    //Variables in calcManager:

    //public bool updateOverlay = false;
    //internal bool toExport = false;

    //called by calculatorManager on start
    protected override void Initialize()
    {
        _instance = this;
        expressions = Expressions._instance;

        vecField = CustomVectorField._instance;
        calcInput = CalcInput._instance;
        //boundsManager = BoundsManager._instance;
        outputManager = OutputManager._instance;
        //saveLoadMenu = SaveLoadMenu._instance;
        //presetMenu = PresetMenu._instance;

        paramScroll = GameObject.Find("PanelBodyParam").transform.GetComponent<Scroll>();
        joyStickAggregator = paramScroll.GetComponent<JoyStickAggregator>();

        //if (boundsManager != null) boundsManager.Initialize(this);
        calcInput.Initialize(this);

        calcInput.ChangeOutput(expressionSet.GetExpression("X"), this); //need to fix
        if (outputManager != null)
        {
            print("OUTPUT INIIALIZED");
            outputManager.Initialize(null, this);
        }
        //presetMenu.Initialize(this);
        //saveLoadMenu.Initialize(this);

        ColorUtility.TryParseHtmlString("#64C3A7FF", out positiveFeedback);

        //if (connectedMenus.particleAnimationSettings != null)
        //    connectedMenus.particleAnimationSettings.Initialize(this);
    }

    private void addForwarders(Transform obj)
    {
        JoyStickForwarder[] forwarders = obj.GetComponentsInChildren<JoyStickForwarder>();
        foreach (JoyStickForwarder j in forwarders)
        {
            joyStickAggregator.AddForwarder(j);
        }
    }

    public void PresetPressed()
    {
        calcInput.ChangeOutput(expressionSet.GetExpression("X"), this); //need to fix
        //if (boundsManager != null) boundsManager.UpdateButtonText();
        inputReceived = true;
    }



    public void LoadSavedExpressionSets(List<ExpressionSet> expressionSets)
    {
        List<ExpressionSet> ess = new List<ExpressionSet>();
        for (int i = 0; i < expressionSets.Count; i++)
        {
            ess.Add(expressionSets[i].DeepCopy());
            ess[ess.Count - 1].CompileAll();
        }
        vecField.expressionSets = ess;
        expressionSet = vecField.expressionSets[0];
        calcInput.ChangeOutput(expressionSet.GetExpression("X"), this); //need to fix
        //if (boundsManager != null) boundsManager.UpdateButtonText();
        inputReceived = true;
    }

    public override void SetOutput(CalcOutput output)
    {
        calcInput.ChangeOutput(output, this);
        inputReceived = true;
    }

    private string getExpOption()
    {
        title = (expressions.getSelectedBody()) ? expressions.getSelectedBody().getTitle() : "X";
        return title;
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
            {
                vecField.UpdateExpressionSet(expressionSetList);
            }
        }
        if (toExport)
        {
            toExport = false;
            vecField.DrawVectorField();
        }
    }

    public override bool letterPressed(string buttonID)
    {
        return false;
    }

    public override void deleteVariables(List<string> toDelete) { }
}
