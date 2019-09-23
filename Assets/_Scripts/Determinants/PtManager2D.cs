using System.Collections;
using System.Collections.Generic;
using UnityEngine;


namespace Determinants
{
    public class PtManager2D : MonoBehaviour
    {	
		
        [HideInInspector]
        public bool inputReceived;
        [HideInInspector]
        public bool eqnInput;


        [HideInInspector]
        public PtSet ptSet;

        [HideInInspector]
        public EqnSet eqnSet;

        private PtInput2D ptInput2D;

        private Color positiveFeedback = new Color(0, 204, 54);
        private Color negativeFeedback = Color.red;//new Color(220,53,69);

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
            internal PtInput2D ptInput2D;

            [SerializeField]
            internal PtOutputMenu2D ptOutputMenu2D;
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
            internal TextMesh  pt1XInput,pt1YInput,// X,Y,Z dimensionns changes in the context of 2D
                    pt2XInput, pt2YInput,
                    aInput;
        }

        public void SetOutput(CalcOutput output)
        {
            ptInput2D.ChangeOutput(output);
        }

        private void Initialize()
        {
            ptInput2D = connectedMenus.ptInput2D;
            connectedMenus.ptInput2D.Initialize(this);  //tmp
            connectedMenus.ptOutputMenu2D.Initialize(this);  //tmp
            ptSet = new PtSet();

            eqnSet = new EqnSet();

            ptInput2D.ChangeOutput(ptSet.ptCoords["pt1"].X);
            updatePoint("pt1", new Vector3(0, 1, 0), false);
            updatePoint("pt2", new Vector3(1, 0, 0), false);
            updatePoint("pt3", new Vector3(0, 0, 0), false); //this line can probably be removed
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
            bool test = ptSet.expValidity["pt1"] & ptSet.expValidity["pt2"];
            if (feedbacks.eqnFeedback != null) feedbacks.eqnFeedback.material.color = test ? positiveFeedback : negativeFeedback;
        }

        public void updatePoint(string ptName, Vector3 newLoc, bool fixedPlane)
        {
            CalcOutput originalExpression = ptInput2D.currExpression;
            SetOutput(ptSet.ptCoords[ptName].X);
            ptInput2D.RewriteInput(newLoc.y);
            SetOutput(ptSet.ptCoords[ptName].Y);
            ptInput2D.RewriteInput(newLoc.x);
            SetOutput(ptSet.ptCoords[ptName].Z);
            ptInput2D.RewriteInput(newLoc.z);
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
		
        public void updateDet(float newDet)//, float newB, float newC, float newD)
        {
            CalcOutput originalExpression = ptInput2D.currExpression;
            SetOutput(eqnSet.eqnCoefs["a"]);
            ptInput2D.RewriteInput(newDet);
            SetOutput(originalExpression);
            manageText();
            eqnSet.CompileAll();
        }

        public void manageText()
        {
            #region coords
            if (ptSet.ptCoords.ContainsKey("pt1") && inputs.pt1YInput != null)
            {
                inputs.pt1XInput.text = displayText(ptSet.ptCoords["pt1"].X.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt1"].X, maxDisplayLength); 
                inputs.pt1YInput.text = displayText(ptSet.ptCoords["pt1"].Y.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt1"].Y, maxDisplayLength);

                if (inputs.pt1XInput.text.Length == 0) inputs.pt1XInput.text = "0";
                if (inputs.pt1YInput.text.Length == 0) inputs.pt1YInput.text = "0";
            }
            if (ptSet.ptCoords.ContainsKey("pt2") && inputs.pt2YInput != null)
            {
                inputs.pt2XInput.text = displayText(ptSet.ptCoords["pt2"].X.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt2"].X, maxDisplayLength); //TAG
                inputs.pt2YInput.text = displayText(ptSet.ptCoords["pt2"].Y.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt2"].Y, maxDisplayLength);

                if (inputs.pt2XInput.text.Length == 0) inputs.pt2XInput.text = "0"; 
                if (inputs.pt2YInput.text.Length == 0) inputs.pt2YInput.text = "0";
            }
			 
            if (eqnSet.eqnCoefs.ContainsKey("a") && inputs.aInput != null) inputs.aInput.text = displayText(eqnSet.eqnCoefs["a"].tokens, ptInput2D.index, ptInput2D.currExpression == eqnSet.eqnCoefs["a"], maxEqnLength);

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

