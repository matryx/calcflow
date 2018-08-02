using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VecFieldManager : CalculatorManager
{
    //[HideInInspector]
    //public bool inputReceived;

    public static VecFieldManager _instance;
    Scroll vecScroll;
    CustomVectorField vecField;
    //BoundsManager boundsManager;
    PresetMenu presetMenu;
    SaveLoadMenu saveLoadMenu;
    OutputManager outputManager;

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

        vecScroll = GameObject.Find("PanelBodyParam").transform.GetComponent<Scroll>();
        joyStickAggregator = vecScroll.GetComponent<JoyStickAggregator>();

        //if (boundsManager != null) boundsManager.Initialize(this);
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

    public FlowLineParticles flowline;

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
                flowline.ForceUpdate();
            }
        }
    }

    public override void deleteVariables(List<string> toDelete) { }
}
