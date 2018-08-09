using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParametricManager : CalculatorManager
{
    public static ParametricManager _instance;
    Scroll paramScroll;
    CustomParametrizedSurface paramSurface;
    BoundsManager boundsManager;
 
    OutputManager outputManager;
  
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

        calcInput.ChangeOutput(expressionSet.GetExpression("X"), this); //need to fix
        if (outputManager != null)
        {
            print("OUTPUT INIIALIZED");
            outputManager.Initialize();
        }
        //presetMenu.Initialize(this);
        //saveLoadMenu.Initialize(this);

        ColorUtility.TryParseHtmlString("#64C3A7FF", out positiveFeedback);

        //if (connectedMenus.particleAnimationSettings != null)
        //    connectedMenus.particleAnimationSettings.Initialize(this);
    }

    public void PresetPressed()
    {
        calcInput.ChangeOutput(expressionSet.GetExpression("X"), this); //need to fix
        if (boundsManager != null) boundsManager.UpdateButtonText();
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
        calcInput.ChangeOutput(expressionSet.GetExpression("X"), this); //need to fix
        if (boundsManager != null) boundsManager.UpdateButtonText();
        inputReceived = true;
    }

    public override void DeleteVariables(List<string> toDelete)
    {
        foreach (string del in toDelete)
        {
            expressionSet.RemoveVariable(del);
        }

        expressions.GetSelectedExpr().GetComponent<ParametricExpression>().DeleteVariable(toDelete);
    }

    void Update()
    {
        if (inputReceived)
        {
            ManageText();
            inputReceived = false;
            updateOverlay = true;
            bool isValid;

            if (expressionSet != null)
            {
                isValid = expressionSet.CompileAll();
                ManageFeedback();
            }
            else
            {
                isValid = true;
            }

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
}
