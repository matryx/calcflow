using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PtManager : MonoBehaviour
{
    [HideInInspector]
    public bool inputReceived;

    [HideInInspector]
    public PtSet ptSet;

    [HideInInspector]
    public EqnSet eqnSet;    

    [HideInInspector]
    public SaveLoadMenu saveLoadMenu;

    private PtInput ptInput;

    private Color positiveFeedback = new Color(0, 204, 54);
    private Color negativeFeedback = Color.red;

    int maxDisplayLength = 7;
    
    int maxEqnLength = 6;

    [SerializeField]
    ConnectedMenus connectedMenus;

    [SerializeField]
    FeedBacks feedbacks;

    [SerializeField]
    Inputs inputs;

    public TextMesh equation;

    [SerializeField]
    PresentPlane presentPlane;

    [System.Serializable]
    internal class ConnectedMenus
    {
        [SerializeField]
        internal PtInput ptInput;

        [SerializeField]
        internal PtOutputMenu ptOutputMenu;
    }

    [System.Serializable]
    internal class FeedBacks
    {
        [SerializeField]
        internal Renderer pt1Feedback;
        [SerializeField]
        internal Renderer pt2Feedback;
        [SerializeField]
        internal Renderer pt3Feedback;
        [SerializeField]
        internal Renderer eqnFeedback;
    }

    [System.Serializable]
    internal class Inputs
    {
        [SerializeField]
        internal TextMesh pt1XInput, pt1YInput, pt1ZInput,
                pt2XInput, pt2YInput, pt2ZInput,
                pt3XInput, pt3YInput, pt3ZInput,
                aInput, bInput, cInput, dInput;
    }

    public bool eqnInput = false;
    public GeneratePlanePts generatePlanePts;

    public void SetOutput(CalcOutput output)
    {
        ptInput.ChangeOutput(output);
        if (output != eqnSet.eqnCoefs["a"] && output != eqnSet.eqnCoefs["b"] && output != eqnSet.eqnCoefs["c"] && output != eqnSet.eqnCoefs["d"]) {
            eqnInput = false;
        } else {
            eqnInput = true;
        }
    }

    private void Initialize()
    {
        ptInput = connectedMenus.ptInput;
        connectedMenus.ptInput.Initialize(this);
        connectedMenus.ptOutputMenu.Initialize(this);
        ptSet = new PtSet();
        eqnSet = new EqnSet();
        ptInput.ChangeOutput(ptSet.ptCoords["pt1"].X);
    }


    void Awake()
    {
        Initialize();
        inputReceived = true;
    }
    public bool updateText = false;

    void Update()
    {
        if (updateText || inputReceived)
        {
            manageText();
            updateText = false;
        }

        if (inputReceived && !eqnInput)
        {
            inputReceived = false;
            bool isValid = ptSet.CompileAll();
            
            ManageFeedback();
            if (isValid) {
                if (presentPlane.CalculatePlane()) {
                    presentPlane.ApplyGraphAdjustment(true);
                    presentPlane.GetLocalPoint();
                    presentPlane.GetPlaneDirection();
                } else {
                    presentPlane.GetLocalPoint();
                }
            }
        }

        if (inputReceived && eqnInput)
        {
            inputReceived = false;
            bool isValid = eqnSet.CompileAll();
            ManageFeedback();
            if (isValid) {
                if (eqnSet.eqnCoefs["a"].Value == 0 && eqnSet.eqnCoefs["b"].Value == 0 && eqnSet.eqnCoefs["c"].Value == 0)
                {
                    feedbacks.eqnFeedback.material.color = negativeFeedback;
                    presentPlane.forwardPlane.GetComponent<MeshRenderer>().enabled = false;
                    presentPlane.backwardPlane.GetComponent<MeshRenderer>().enabled = false;
                }
                else
                {
                    generatePlanePts.a = eqnSet.eqnCoefs["a"].Value;
                    generatePlanePts.b = eqnSet.eqnCoefs["b"].Value;
                    generatePlanePts.c = eqnSet.eqnCoefs["c"].Value;
                    generatePlanePts.d = eqnSet.eqnCoefs["d"].Value;
                    generatePlanePts.eqnToPoints();
                }
            }
        }
    }

    public void ManageFeedback()
    {
        if (feedbacks.pt1Feedback != null) feedbacks.pt1Feedback.material.color = ptSet.expValidity["pt1"] ? positiveFeedback : negativeFeedback;
        if (feedbacks.pt2Feedback != null) feedbacks.pt2Feedback.material.color = ptSet.expValidity["pt2"] ? positiveFeedback : negativeFeedback;
        if (feedbacks.pt3Feedback != null) feedbacks.pt3Feedback.material.color = ptSet.expValidity["pt3"] ? positiveFeedback : negativeFeedback;
        if (feedbacks.eqnFeedback != null) feedbacks.eqnFeedback.material.color = eqnSet.coefValidity ? positiveFeedback : negativeFeedback;
    }

    public void updatePoint(string ptName, Vector3 newLoc, bool fixedPlane) 
    {
        CalcOutput originalExpression = ptInput.currExpression;
        eqnInput = false;
        SetOutput(ptSet.ptCoords[ptName].X);
        ptInput.RewriteInput(newLoc.x);
        SetOutput(ptSet.ptCoords[ptName].Y);
        ptInput.RewriteInput(newLoc.y);
        SetOutput(ptSet.ptCoords[ptName].Z);
        ptInput.RewriteInput(newLoc.z);
        SetOutput(originalExpression);
        if (fixedPlane) 
        {
            manageText();
            ptSet.CompileAll();
        } else 
        {
            manageText();
            bool isValid = ptSet.CompileAll();
            ManageFeedback();
            if (isValid) 
            {
                if (presentPlane.CalculatePlane()) {
                    presentPlane.ApplyUnroundCenter(ptName, newLoc);
                    presentPlane.GetPlaneDirection();
                }
            }
        }
    }

    public void eqnUpdatePoint(Vector3 pt1NewLoc, Vector3 pt2NewLoc, Vector3 pt3NewLoc)
    {
        CalcOutput originalExpression = ptInput.currExpression;
        SetOutput(ptSet.ptCoords["pt1"].X);
        ptInput.RewriteInput(pt1NewLoc.x);
        SetOutput(ptSet.ptCoords["pt1"].Y);
        ptInput.RewriteInput(pt1NewLoc.y);
        SetOutput(ptSet.ptCoords["pt1"].Z);
        ptInput.RewriteInput(pt1NewLoc.z);
        SetOutput(ptSet.ptCoords["pt2"].X);
        ptInput.RewriteInput(pt2NewLoc.x);
        SetOutput(ptSet.ptCoords["pt2"].Y);
        ptInput.RewriteInput(pt2NewLoc.y);
        SetOutput(ptSet.ptCoords["pt2"].Z);
        ptInput.RewriteInput(pt2NewLoc.z);
        SetOutput(ptSet.ptCoords["pt3"].X);
        ptInput.RewriteInput(pt3NewLoc.x);
        SetOutput(ptSet.ptCoords["pt3"].Y);
        ptInput.RewriteInput(pt3NewLoc.y);
        SetOutput(ptSet.ptCoords["pt3"].Z);
        ptInput.RewriteInput(pt3NewLoc.z);
        SetOutput(originalExpression);
        manageText();
        ptSet.CompileAll();
        presentPlane.GetLocalPoint();
        presentPlane.GetPlaneDirection();
        presentPlane.forwardPlane.GetComponent<MeshRenderer>().enabled = true;
		presentPlane.backwardPlane.GetComponent<MeshRenderer>().enabled = true;
    }

    public void updateEqn(float newA, float newB, float newC, float newD) 
    {
        CalcOutput originalExpression = ptInput.currExpression;
        SetOutput(eqnSet.eqnCoefs["a"]);
        ptInput.RewriteInput(newA);
        SetOutput(eqnSet.eqnCoefs["b"]);
        ptInput.RewriteInput(newB);
        SetOutput(eqnSet.eqnCoefs["c"]);
        ptInput.RewriteInput(newC);
        SetOutput(eqnSet.eqnCoefs["d"]);
        ptInput.RewriteInput(newD);
        SetOutput(originalExpression);
        manageText();
        eqnSet.CompileAll();
        ManageFeedback();        
    }

    public void updateEqn()
    {
        CalcOutput originalExpression = ptInput.currExpression;
        SetOutput(eqnSet.eqnCoefs["a"]);
        ptInput.RewriteInput();
        SetOutput(eqnSet.eqnCoefs["b"]);
        ptInput.RewriteInput();
        SetOutput(eqnSet.eqnCoefs["c"]);
        ptInput.RewriteInput();
        SetOutput(eqnSet.eqnCoefs["d"]);
        ptInput.RewriteInput();
        SetOutput(originalExpression);
        manageText();
        eqnSet.CompileAll();
        inputs.aInput.text = "NaN";
        inputs.bInput.text = "NaN";
        inputs.cInput.text = "NaN";
        inputs.dInput.text = "NaN";
        feedbacks.eqnFeedback.material.color = negativeFeedback;
    }

    public void manageText()
    {
        #region coords
        if (ptSet.ptCoords.ContainsKey("pt1") && inputs.pt1XInput != null)
        {
            inputs.pt1XInput.text = displayText(ptSet.ptCoords["pt1"].X.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt1"].X, maxDisplayLength);
            inputs.pt1YInput.text = displayText(ptSet.ptCoords["pt1"].Y.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt1"].Y, maxDisplayLength);
            inputs.pt1ZInput.text = displayText(ptSet.ptCoords["pt1"].Z.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt1"].Z, maxDisplayLength);

            if (inputs.pt1XInput.text.Length == 0) inputs.pt1XInput.text = "0";
            if (inputs.pt1YInput.text.Length == 0) inputs.pt1YInput.text = "0";
            if (inputs.pt1ZInput.text.Length == 0) inputs.pt1ZInput.text = "0";
        }
        if (ptSet.ptCoords.ContainsKey("pt2") && inputs.pt2XInput != null)
        {
            inputs.pt2XInput.text = displayText(ptSet.ptCoords["pt2"].X.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt2"].X, maxDisplayLength);
            inputs.pt2YInput.text = displayText(ptSet.ptCoords["pt2"].Y.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt2"].Y, maxDisplayLength);
            inputs.pt2ZInput.text = displayText(ptSet.ptCoords["pt2"].Z.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt2"].Z, maxDisplayLength);

            if (inputs.pt2XInput.text.Length == 0) inputs.pt2XInput.text = "0";
            if (inputs.pt2YInput.text.Length == 0) inputs.pt2YInput.text = "0";
            if (inputs.pt2ZInput.text.Length == 0) inputs.pt2ZInput.text = "0";
        }
        if (ptSet.ptCoords.ContainsKey("pt3") && inputs.pt3XInput != null)
        {
            inputs.pt3XInput.text = displayText(ptSet.ptCoords["pt3"].X.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt3"].X, maxDisplayLength);
            inputs.pt3YInput.text = displayText(ptSet.ptCoords["pt3"].Y.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt3"].Y, maxDisplayLength);
            inputs.pt3ZInput.text = displayText(ptSet.ptCoords["pt3"].Z.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt3"].Z, maxDisplayLength);

            if (inputs.pt3XInput.text.Length == 0) inputs.pt3XInput.text = "0";
            if (inputs.pt3YInput.text.Length == 0) inputs.pt3YInput.text = "0";
            if (inputs.pt3ZInput.text.Length == 0) inputs.pt3ZInput.text = "0";
        }
        if (eqnSet.eqnCoefs.ContainsKey("a") && inputs.aInput != null) inputs.aInput.text = displayText(eqnSet.eqnCoefs["a"].tokens, ptInput.index, ptInput.currExpression == eqnSet.eqnCoefs["a"], maxEqnLength);
        if (eqnSet.eqnCoefs.ContainsKey("b") && inputs.bInput != null) inputs.bInput.text = displayText(eqnSet.eqnCoefs["b"].tokens, ptInput.index, ptInput.currExpression == eqnSet.eqnCoefs["b"], maxEqnLength);
        if (eqnSet.eqnCoefs.ContainsKey("c") && inputs.cInput != null) inputs.cInput.text = displayText(eqnSet.eqnCoefs["c"].tokens, ptInput.index, ptInput.currExpression == eqnSet.eqnCoefs["c"], maxEqnLength);
        if (eqnSet.eqnCoefs.ContainsKey("d") && inputs.dInput != null) inputs.dInput.text = displayText(eqnSet.eqnCoefs["d"].tokens, ptInput.index, ptInput.currExpression == eqnSet.eqnCoefs["d"], maxEqnLength);
        
        if (inputs.aInput != null && inputs.aInput.text.Length == 0) inputs.aInput.text = "0";
        if (inputs.bInput != null && inputs.bInput.text.Length == 0) inputs.bInput.text = "0";
        if (inputs.cInput != null && inputs.cInput.text.Length == 0) inputs.cInput.text = "0";
        if (inputs.dInput != null && inputs.dInput.text.Length == 0) inputs.dInput.text = "0";
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
