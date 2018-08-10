using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CalcManager : MonoBehaviour
{
    ExpressionSet currExpressionSet;
    CalcOutput currOutput;

    [HideInInspector]
    public bool inputReceived;

    [HideInInspector]
    public ExpressionSet expressionSet;

    [HideInInspector]
    public SaveLoadMenu saveLoadMenu;

    public CustomParametrizedSurface paramSurface;
    private CalcInput calcInput;
    private PieceWiseControl pieceWiseControl;
    private BoundsManager boundsManager;

    private Color positiveFeedback = new Color(0, 204, 54);
    private Color negativeFeedback = Color.red;

    int maxDisplayLength = 20;
    int rangeDisplayLength = 5;

    internal bool toExport = false;

    [SerializeField]
    public FlexActionableComponent defaultSpeed;
    public FlexActionableComponent defaultEffect;

    [SerializeField]
    ConnectedMenus connectedMenus;

    [SerializeField]
    FeedBacks feedbacks;

    [SerializeField]
    Inputs inputs;

    //some aren't needed anymore
    //shouldn't be public anymore
    [System.Serializable]
    internal class ConnectedMenus
    {
        [SerializeField]
        internal CalcInput calcInput;
        [SerializeField]
        internal PieceWiseControl pieceWiseControl; // dont need in new system
        [SerializeField]
        internal PresetMenu presetMenu;
        [SerializeField]
        internal OutputMenu outputMenu;             // dont need in new system
        [SerializeField]
        internal BoundsManager boundsManager;
        [SerializeField]
        internal SaveLoadMenu saveLoadMenu;
        [SerializeField]
        internal ParticleAnimationSettings particleAnimationSettings;  // dont need in new system
    }

    //no longer backboards, these are now underline (google form)
    //shouldn't be public anymore
    [System.Serializable]
    internal class FeedBacks
    {
        [SerializeField]
        internal Renderer xFeedback;
        [SerializeField]
        internal Renderer yFeedback;
        [SerializeField]
        internal Renderer zFeedback;
        [SerializeField]
        internal Renderer tFeedback;
        [SerializeField]
        internal Renderer uFeedback;
        [SerializeField]
        internal Renderer vFeedback;
        [SerializeField]
        internal Renderer wFeedback;
    }

    //this all has to change since it isn't one set of output anymore (multiple sets of expressions in new system)
    //shouldn't be public anymore
    [System.Serializable]
    internal class Inputs
    {
        [SerializeField]
        internal TextMesh xInputbox, yInputbox, zInputbox,
                tMinInput, tMaxInput,
                uMinInput, uMaxInput,
                vMinInput, vMaxInput,
                wMinInput, wMaxInput;
    }

    public void ChangeExpressionSet(ExpressionSet ES)
    {
        expressionSet = ES;
        //calcInput.ChangeOutput(expressionSet.expressions["X"]);
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
        paramSurface.ExpressionSets = ess;
        pieceWiseControl.ForceNumberOfTabs(ess.Count);
        expressionSet = paramSurface.ExpressionSets[0];
        //calcInput.ChangeOutput(expressionSet.expressions["X"]);
        if(boundsManager != null) boundsManager.UpdateButtonText();
        inputReceived = true;
    }

    public void SetOutput(CalcOutput output)
    {
        //calcInput.ChangeOutput(output);
    }

    private void Initialize()
    {
        calcInput = connectedMenus.calcInput;
        boundsManager = connectedMenus.boundsManager;
        pieceWiseControl = connectedMenus.pieceWiseControl;
        boundsManager = connectedMenus.boundsManager;
        saveLoadMenu = connectedMenus.saveLoadMenu;

        //if (connectedMenus.boundsManager != null) connectedMenus.boundsManager.Initialize(this);
        //connectedMenus.calcInput.Initialize(this);
        //tier 3
        //connectedMenus.outputMenu.Initialize(this);

        //Req: calcInput
        connectedMenus.pieceWiseControl.Initialize(this);
        //Req: calcInput
        //calcInput.ChangeOutput(expressionSet.expressions["X"]);
        //Req: calcInput
        //connectedMenus.presetMenu.Initialize(this);

        //connectedMenus.saveLoadMenu.Initialize(this);

        if (connectedMenus.particleAnimationSettings != null)
            connectedMenus.particleAnimationSettings.Initialize(this);
    }

    public void PresetPressed()
    {
        //calcInput.ChangeOutput(expressionSet.expressions["X"]);
        if (boundsManager != null) boundsManager.UpdateButtonText();
        inputReceived = true;
    }

    // Use this for initialization
    void Awake()
    {
        Initialize();
    }


    public bool updateOverlay = false;
    // Update is called once per frame
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
            SurfaceTessellation._instance.GenerateMesh(paramSurface.ExpressionSets);
        }
    }

    public void ManageFeedback()
    {
        if (feedbacks.xFeedback != null) feedbacks.xFeedback.material.color = expressionSet.expValidity["X"] ? positiveFeedback : negativeFeedback;
        if (feedbacks.yFeedback != null) feedbacks.yFeedback.material.color = expressionSet.expValidity["Y"] ? positiveFeedback : negativeFeedback;
        if (feedbacks.zFeedback != null) feedbacks.zFeedback.material.color = expressionSet.expValidity["Z"] ? positiveFeedback : negativeFeedback;
        if (feedbacks.tFeedback != null) feedbacks.tFeedback.material.color = expressionSet.expValidity["t"] ? positiveFeedback : negativeFeedback;
        if (feedbacks.uFeedback != null) feedbacks.uFeedback.material.color = expressionSet.expValidity["u"] ? positiveFeedback : negativeFeedback;
        if (feedbacks.vFeedback != null) feedbacks.vFeedback.material.color = expressionSet.expValidity["v"] ? positiveFeedback : negativeFeedback;
        if (feedbacks.wFeedback != null) feedbacks.wFeedback.material.color = expressionSet.expValidity["w"] ? positiveFeedback : negativeFeedback;
    }

    public void manageText()
    {
        #region expressions
        if (expressionSet.GetExpression("X") != null && inputs.xInputbox != null)
        {
            inputs.xInputbox.text = displayText(expressionSet.GetExpression("X").tokens, calcInput.index, calcInput.currExpression == expressionSet.GetExpression("X"), maxDisplayLength);
        }
        if (expressionSet.GetExpression("Y") != null && inputs.yInputbox != null)
        {
            inputs.yInputbox.text = displayText(expressionSet.GetExpression("Y").tokens, calcInput.index, calcInput.currExpression == expressionSet.GetExpression("Y"), maxDisplayLength);
        }
        if (expressionSet.GetExpression("Z") != null && inputs.zInputbox != null)
        {
            inputs.zInputbox.text = displayText(expressionSet.GetExpression("Z").tokens, calcInput.index, calcInput.currExpression == expressionSet.GetExpression("Z"), maxDisplayLength);
        }
        #endregion
        #region params
        if (expressionSet.GetRange("t") != null && inputs.tMinInput != null)
        {
            inputs.tMinInput.text = displayText(expressionSet.GetRange("t").Min.tokens, calcInput.index, calcInput.currExpression == expressionSet.GetRange("t").Min, rangeDisplayLength);
            inputs.tMaxInput.text = displayText(expressionSet.GetRange("t").Max.tokens, calcInput.index, calcInput.currExpression == expressionSet.GetRange("t").Max, rangeDisplayLength);
        }
        if (expressionSet.GetRange("u") != null && inputs.uMinInput != null)
        {
            inputs.uMinInput.text = displayText(expressionSet.GetRange("u").Min.tokens, calcInput.index, calcInput.currExpression == expressionSet.GetRange("u").Min, rangeDisplayLength);
            inputs.uMaxInput.text = displayText(expressionSet.GetRange("u").Max.tokens, calcInput.index, calcInput.currExpression == expressionSet.GetRange("u").Max, rangeDisplayLength);
        }
        if (expressionSet.GetRange("v") != null && inputs.vMinInput != null)
        {
            inputs.vMinInput.text = displayText(expressionSet.GetRange("v").Min.tokens, calcInput.index, calcInput.currExpression == expressionSet.GetRange("v").Min, rangeDisplayLength);
            inputs.vMaxInput.text = displayText(expressionSet.GetRange("v").Max.tokens, calcInput.index, calcInput.currExpression == expressionSet.GetRange("v").Max, rangeDisplayLength);
        }
        if (expressionSet.GetRange("w") != null && inputs.wMinInput != null)
        {
            inputs.wMinInput.text = displayText(expressionSet.GetRange("w").Min.tokens, calcInput.index, calcInput.currExpression == expressionSet.GetRange("w").Min, rangeDisplayLength);
            inputs.wMaxInput.text = displayText(expressionSet.GetRange("w").Max.tokens, calcInput.index, calcInput.currExpression == expressionSet.GetRange("w").Max, rangeDisplayLength);
        }
        #endregion

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

}
