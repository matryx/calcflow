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
    public SaveLoadMenu saveLoadMenu;

    private PtInput ptInput;

    private Color positiveFeedback = new Color(0, 204, 54);
    private Color negativeFeedback = Color.red;

    int maxDisplayLength = 6;

    [SerializeField]
    public FlexActionableComponent defaultSpeed;
    public FlexActionableComponent defaultEffect;

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
    }

    [System.Serializable]
    internal class Inputs
    {
        [SerializeField]
        internal TextMesh pt1XInput, pt1YInput, pt1ZInput,
                pt2XInput, pt2YInput, pt2ZInput,
                pt3XInput, pt3YInput, pt3ZInput;
    }

    public void SetOutput(CalcOutput output)
    {
        ptInput.ChangeOutput(output);
    }

    private void Initialize()
    {
        ptInput = connectedMenus.ptInput;
        connectedMenus.ptInput.Initialize(this);
        connectedMenus.ptOutputMenu.Initialize(this);
        ptSet = new PtSet();
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

        if (inputReceived)
        {
            inputReceived = false;
            bool isValid = ptSet.CompileAll();
            ManageFeedback();
            if (isValid) {
                equation.text = presentPlane.CalculatePlane();
			    presentPlane.ApplyGraphAdjustment(true);
                presentPlane.GetLocalPoint();
                presentPlane.GetPlaneDirection();
            } else {
                equation.text = "Invalid Plane";
            }
        }
    }

    public void ManageFeedback()
    {
        if (feedbacks.pt1Feedback != null) feedbacks.pt1Feedback.material.color = ptSet.expValidity["pt1"] ? positiveFeedback : negativeFeedback;
        if (feedbacks.pt2Feedback != null) feedbacks.pt2Feedback.material.color = ptSet.expValidity["pt2"] ? positiveFeedback : negativeFeedback;
        if (feedbacks.pt3Feedback != null) feedbacks.pt3Feedback.material.color = ptSet.expValidity["pt3"] ? positiveFeedback : negativeFeedback;
    }

    public void updatePoint(string ptName, Vector3 newLoc, bool fixedPlane) {
        CalcOutput originalExpression = ptInput.currExpression;
        SetOutput(ptSet.ptCoords[ptName].X);
        ptInput.RewriteInput(newLoc.x);
        SetOutput(ptSet.ptCoords[ptName].Y);
        ptInput.RewriteInput(newLoc.y);
        SetOutput(ptSet.ptCoords[ptName].Z);
        ptInput.RewriteInput(newLoc.z);
        SetOutput(originalExpression);
        if (fixedPlane) {
            manageText();
            ptSet.CompileAll();
            inputReceived = false;
        } else {
            manageText();
            inputReceived = false;
            bool isValid = ptSet.CompileAll();
            ManageFeedback();
            if (isValid) {
                equation.text = presentPlane.CalculatePlane();
                presentPlane.ApplyUnroundCenter(ptName, newLoc);
                presentPlane.GetPlaneDirection();
            } else {
                equation.text = "Invalid Plane";
            }
        }
    }

    public void manageText()
    {
        #region coords
        if (ptSet.ptCoords.ContainsKey("pt1") && inputs.pt1XInput != null)
        {
            inputs.pt1XInput.text = displayText(ptSet.ptCoords["pt1"].X.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt1"].X, maxDisplayLength);
            inputs.pt1YInput.text = displayText(ptSet.ptCoords["pt1"].Y.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt1"].Y, maxDisplayLength);
            inputs.pt1ZInput.text = displayText(ptSet.ptCoords["pt1"].Z.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt1"].Z, maxDisplayLength);
        }
        if (ptSet.ptCoords.ContainsKey("pt2") && inputs.pt2XInput != null)
        {
            inputs.pt2XInput.text = displayText(ptSet.ptCoords["pt2"].X.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt2"].X, maxDisplayLength);
            inputs.pt2YInput.text = displayText(ptSet.ptCoords["pt2"].Y.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt2"].Y, maxDisplayLength);
            inputs.pt2ZInput.text = displayText(ptSet.ptCoords["pt2"].Z.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt2"].Z, maxDisplayLength);
        }
        if (ptSet.ptCoords.ContainsKey("pt3") && inputs.pt3XInput != null)
        {
            inputs.pt3XInput.text = displayText(ptSet.ptCoords["pt3"].X.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt3"].X, maxDisplayLength);
            inputs.pt3YInput.text = displayText(ptSet.ptCoords["pt3"].Y.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt3"].Y, maxDisplayLength);
            inputs.pt3ZInput.text = displayText(ptSet.ptCoords["pt3"].Z.tokens, ptInput.index, ptInput.currExpression == ptSet.ptCoords["pt3"].Z, maxDisplayLength);
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
