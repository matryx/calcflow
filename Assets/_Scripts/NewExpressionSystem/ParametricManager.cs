using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParametricManager : CalculatorManager
{
    [HideInInspector]
    public ExpressionSet expressionSet;

    //[HideInInspector]
    //public bool inputReceived;

    public static ParametricManager _instance;
    JoyStickAggregator joyStickAggregator;
    Scroll paramScroll;
    CustomParametrizedSurface paramSurface;
    List<ExpressionSet> expressionSetList = new List<ExpressionSet>();
    BoundsManager boundsManager;
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

        paramSurface = CustomParametrizedSurface._instance;
        calcInput = CalcInput._instance;
        boundsManager = BoundsManager._instance;
        outputManager = OutputManager._instance;
        //saveLoadMenu = SaveLoadMenu._instance;
        //presetMenu = PresetMenu._instance;

        paramScroll = GameObject.Find("PanelBodyParam").transform.GetComponent<Scroll>();
        joyStickAggregator = paramScroll.GetComponent<JoyStickAggregator>();

        if (boundsManager != null) boundsManager.Initialize(this);
        calcInput.Initialize(this);

        calcInput.ChangeOutput(expressionSet.expressions["X"], this); //need to fix
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
        calcInput.ChangeOutput(expressionSet.expressions["X"], this); //need to fix
        if (boundsManager != null) boundsManager.UpdateButtonText();
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
        calcInput.ChangeOutput(expressionSet.expressions["X"], this); //need to fix
        if (boundsManager != null) boundsManager.UpdateButtonText();
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
                paramSurface.UpdateExpressionSet(expressionSetList);
                paramSurface.GenerateParticles();
            }
        }
        if (toExport)
        {
            toExport = false;
            paramSurface.GenerateMesh();
        }
    }


    public override bool letterPressed(string buttonID)
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
                calcInput.currExpression.expSet.ReAddVariable(buttonID);
                param.GetComponent<ParametricExpression>().addVariable(buttonID, null);
            }
            else if (!expressionSet.ranges.ContainsKey(buttonID))
            {
                GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
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

    public override void deleteVariables(List<string> toDelete)
    {
        foreach (string del in toDelete)
        {
            expressionSet.RemoveVariable(del);
        }

        expressions.getSelectedExpr().GetComponent<ParametricExpression>().deleteVariable(toDelete);
    }
}
