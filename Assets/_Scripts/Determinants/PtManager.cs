using System.Collections;
using System.Collections.Generic;
using UnityEngine;


namespace Determinants
{
    public class PtManager : MonoBehaviour
    {
        [HideInInspector]
        public bool inputReceived;
        [HideInInspector]
        public bool eqnInput;


        [HideInInspector]
        public PtSet ptSet;

        [HideInInspector]
        public EqnSet eqnSet;

        private PtInput ptInput;

        private Color positiveFeedback = new Color(0, 204, 54);
        private Color negativeFeedback = Color.red;

        int maxDisplayLength = 9;

        int maxEqnLength = 7;

        [SerializeField]
        ConnectedMenus connectedMenus;

        [SerializeField]
        FeedBacks feedbacks;

        [SerializeField]
        Inputs inputs;

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
            internal Renderer row1Feedback;
            [SerializeField]
            internal Renderer row2Feedback;
            [SerializeField]
            internal Renderer row3Feedback;
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
                    aInput;
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

            eqnSet = new EqnSet();

            ptInput.ChangeOutput(ptSet.ptCoords["pt1"].X);
            updatePoint("pt1", new Vector3(0, 1, 0), false);
            updatePoint("pt2", new Vector3(1, 0, 0), false);
            updatePoint("pt3", new Vector3(0, 0, 1), false);
        }


        void Start()
        {
            inputReceived = true;
            Initialize();
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

                ManageFeedback(); //keep this?
                if (isValid)
                {
                    if (presentPlane)
                    {
                        presentPlane.CalculatePlane();
                        presentPlane.ApplyGraphAdjustment();
                        presentPlane.GetLocalPoint();
                    }
                }
            }
        }

        public void ManageFeedback()
        {
            bool test = ptSet.expValidity["pt1"] & ptSet.expValidity["pt2"] & ptSet.expValidity["pt3"];
            if (feedbacks.eqnFeedback != null) feedbacks.eqnFeedback.material.color = test ? positiveFeedback : negativeFeedback;
        }

        public void updatePoint(string ptName, Vector3 newLoc, bool fixedPlane)
        {
            CalcOutput originalExpression = ptInput.currExpression;
            SetOutput(ptSet.ptCoords[ptName].X);
            ptInput.RewriteInput(newLoc.y);
            SetOutput(ptSet.ptCoords[ptName].Y);
            ptInput.RewriteInput(newLoc.x);
            SetOutput(ptSet.ptCoords[ptName].Z);
            ptInput.RewriteInput(newLoc.z);
            SetOutput(originalExpression);
            if (fixedPlane)
            {
                manageText();
                ManageFeedback();
                ptSet.CompileAll();
            }
            else
            {
                manageText();
                bool isValid = ptSet.CompileAll();
                ManageFeedback();
            }
        }

        public void updateDet(float newDet)
        {
            CalcOutput originalExpression = ptInput.currExpression;
            SetOutput(eqnSet.eqnCoefs["a"]);
            ptInput.RewriteInput(newDet);
            SetOutput(originalExpression);
            manageText();
            eqnSet.CompileAll();
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

            if (inputs.aInput != null && inputs.aInput.text.Length == 0) inputs.aInput.text = "0";
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

}
