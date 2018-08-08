using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VecFieldManager : CalculatorManager
{
    public static VecFieldManager _instance;
    public FlowLineParticles flowline;

    CustomVectorField vecField;
    OutputManager outputManager;

    Scroll vecScroll;
    PresetMenu presetMenu;
    SaveLoadMenu saveLoadMenu;
    
    //called by calculatorManager on start
    protected override void Initialize()
    {
        _instance = this;
        expressions = Expressions._instance;

        vecField = CustomVectorField._instance;
        calcInput = CalcInput._instance;
        outputManager = OutputManager._instance;

        vecScroll = GameObject.Find("PanelBodyParam").transform.GetComponent<Scroll>();
        joyStickAggregator = vecScroll.GetComponent<JoyStickAggregator>();

        calcInput.Initialize(this);

        calcInput.ChangeOutput(expressionSet.GetExpression("X"), this); //need to fix
        if (outputManager != null)
        {
            print("OUTPUT INIIALIZED");
            outputManager.Initialize();
        }

        ColorUtility.TryParseHtmlString("#64C3A7FF", out positiveFeedback);
        flowline = FlowLineParticles._instance;

    }

    public void PresetPressed()
    {
        calcInput.ChangeOutput(expressionSet.GetExpression("X"), this); //need to fix
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
        inputReceived = true;
    }

    void Update()
    {
        if (inputReceived)
        {
            manageText();
            inputReceived = false;
            updateOverlay = true;
            vecField.dens = CustomVectorField.SampleDensity.HIGH;

            bool isValid = expressionSet.CompileAll();
            ManageFeedback();
            if (isValid)
            {
                vecField.es = expressionSet.ShallowCopy();
                vecField.UpdateFunctions();
            }

            if (flowline != null)
            {
                flowline.t_min = expressionSet.GetRange("t").Min.expression;
                flowline.t_max = expressionSet.GetRange("t").Max.expression;
                flowline.ForceUpdate();
            }
        }
    }

    public override void deleteVariables(List<string> toDelete) { }
}
