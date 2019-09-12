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

        //[HideInInspector]
        //public SaveLoadMenu saveLoadMenu;	//TAG

        private PtInput2D ptInput2D;

        private Color positiveFeedback = new Color(0, 204, 54);
        private Color negativeFeedback = Color.red;//new Color(220,53,69);//Color.red;

        int maxDisplayLength = 9;

        int maxEqnLength = 7; 

        [SerializeField]
        ConnectedMenus connectedMenus;

        [SerializeField]
        FeedBacks feedbacks;

        [SerializeField]
        Inputs inputs;

        //public TextMesh equation; //TAG

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
            internal TextMesh  pt1XInput,pt1YInput,//pt1ZInput, //TAG X,Y,Z dimensionns changes in the context of 2D
                    pt2XInput, pt2YInput,//pt2ZInput, //TAG
                    //pt3XInput, pt3YInput, pt3ZInput, //TAG
                    aInput; // bInput, cInput, dInput; //TAG
        }

        //public GeneratePlanePts generatePlanePts; //TAG

        public void SetOutput(CalcOutput output)
        {
            ptInput2D.ChangeOutput(output);
            /* //TAG
            if (output != eqnSet.eqnCoefs["a"] && output != eqnSet.eqnCoefs["b"] && output != eqnSet.eqnCoefs["c"] && output != eqnSet.eqnCoefs["d"])
            {
                eqnInput = false;
            }
            else
            {
                eqnInput = true;
            }
            */
        }

        private void Initialize()
        {
            ptInput2D = connectedMenus.ptInput2D;
            connectedMenus.ptInput2D.Initialize(this);  //tmp
            connectedMenus.ptOutputMenu2D.Initialize(this);  //tmp
            //List<string> neg1 = new List<string>() { "-", "1" }; //TAG
            //List<string> pos1 = new List<string>() { "1" }; //TAG
            ptSet = new PtSet();

            eqnSet = new EqnSet();

            ptInput2D.ChangeOutput(ptSet.ptCoords["pt1"].X);
            updatePoint("pt1", new Vector3(0, 1, 0), false);
            updatePoint("pt2", new Vector3(1, 0, 0), false);
            updatePoint("pt3", new Vector3(0, 0, 0), false); //TAG

        }


        void Start()
        {
            inputReceived = true;
            //eqnInput = false;
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

                ManageFeedback(); //TAG
                if (isValid)
                {
                    if (presentPlane)
                    {
                        //if (presentPlane.CalculatePlane()) //TAG
                        //{
                            presentPlane.CalculatePlane();
                            presentPlane.ApplyGraphAdjustment();
                            presentPlane.GetLocalPoint();
                            presentPlane.GetPlaneDirection();
                        //}
                        /* else
                        {
                            presentPlane.ApplyGraphAdjustment();
                            presentPlane.GetLocalPoint();
                        }*/ //TAG
                     }
                }
            }
            
            /* //TAG
            if (inputReceived && eqnInput)
            {
                inputReceived = false; //TAG
                bool isValid = eqnSet.CompileAll(); //TAG
                ManageFeedback(); //TAG
                if (isValid) //TAG
                {
                    if (eqnSet.eqnCoefs["a"].Value == 0 && eqnSet.eqnCoefs["b"].Value == 0 && eqnSet.eqnCoefs["c"].Value == 0)
                    {
                        feedbacks.eqnFeedback.material.color = negativeFeedback; //TAG
                        //TAG 
                        if (presentPlane)
                        {
                            //presentPlane.forwardPlane.GetComponent<MeshRenderer>().enabled = false;
                            //presentPlane.backwardPlane.GetComponent<MeshRenderer>().enabled = false;
                        }
                    
                    
                    }
                    else
                    {
                        generatePlanePts.eqnToPoints();//TAG
                    }
                } 
            }
            */
        }

        public void ManageFeedback()
        {
            if (feedbacks.row1Feedback != null) feedbacks.row1Feedback.material.color = ptSet.expValidity["pt1"] ? positiveFeedback : negativeFeedback;
            if (feedbacks.row2Feedback != null) feedbacks.row2Feedback.material.color = ptSet.expValidity["pt2"] ? positiveFeedback : negativeFeedback;
            if (feedbacks.row3Feedback != null) feedbacks.row3Feedback.material.color = ptSet.expValidity["pt3"] ? positiveFeedback : negativeFeedback;
            bool test = ptSet.expValidity["pt1"] & ptSet.expValidity["pt2"];
            if (feedbacks.eqnFeedback != null) feedbacks.eqnFeedback.material.color = test ? positiveFeedback : negativeFeedback;
        }

        public void updatePoint(string ptName, Vector3 newLoc, bool fixedPlane)
        {
            CalcOutput originalExpression = ptInput2D.currExpression;
            //eqnInput = false; //TAG
            //inputReceived = true;

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
                ManageFeedback(); //KEEP
                ptSet.CompileAll();
            }
            else
            {
                manageText();
                bool isValid = ptSet.CompileAll(); //KEEP
                ManageFeedback(); //KEEP
                /* //TAG
                if (isValid)
                {
                    if (presentPlane && presentPlane.CalculatePlane())
                    {
                        //presentPlane.ApplyUnroundCenter(ptName, newLoc);  //TAG
                        presentPlane.GetPlaneDirection();
                    }
                }
                */
            }
        }
		
		/*  //TAG
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
            //ManageFeedback(); //TAG
            ptSet.CompileAll();
            presentPlane.GetLocalPoint();
            presentPlane.GetPlaneDirection();
            //presentPlane.forwardPlane.GetComponent<MeshRenderer>().enabled = true; //TAG
            //presentPlane.backwardPlane.GetComponent<MeshRenderer>().enabled = true; //TAG
        }
        */
        public void updateEqn(float newA, float newB, float newC, float newD)
        {
            CalcOutput originalExpression = ptInput2D.currExpression;
            SetOutput(eqnSet.eqnCoefs["a"]);
            ptInput2D.RewriteInput(newA);
            SetOutput(eqnSet.eqnCoefs["b"]);
            ptInput2D.RewriteInput(newB);
            SetOutput(eqnSet.eqnCoefs["c"]);
            ptInput2D.RewriteInput(newC);
            SetOutput(eqnSet.eqnCoefs["d"]);
            ptInput2D.RewriteInput(newD);
            SetOutput(originalExpression);
            manageText();
            eqnSet.CompileAll();
            //ManageFeedback(); //TAG
        }

        public void updateEqn()
        {
            CalcOutput originalExpression = ptInput2D.currExpression;
            SetOutput(eqnSet.eqnCoefs["a"]);
            ptInput2D.RewriteInput();
            SetOutput(eqnSet.eqnCoefs["b"]);
            ptInput2D.RewriteInput();
            SetOutput(eqnSet.eqnCoefs["c"]);
            ptInput2D.RewriteInput();
            SetOutput(eqnSet.eqnCoefs["d"]);
            ptInput2D.RewriteInput();
            SetOutput(originalExpression);
            manageText();
            eqnSet.CompileAll();
            // inputs.aInput.text = "NaN";
            // inputs.bInput.text = "NaN";
            // inputs.cInput.text = "NaN";
            // inputs.dInput.text = "NaN";
            feedbacks.eqnFeedback.material.color = negativeFeedback;
        }
		
        public void manageText()
        {
            #region coords
            if (ptSet.ptCoords.ContainsKey("pt1") && inputs.pt1YInput != null)
            {
                inputs.pt1XInput.text = displayText(ptSet.ptCoords["pt1"].X.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt1"].X, maxDisplayLength); 
                inputs.pt1YInput.text = displayText(ptSet.ptCoords["pt1"].Y.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt1"].Y, maxDisplayLength);
                //inputs.pt1ZInput.text = displayText(ptSet.ptCoords["pt1"].Z.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt1"].Z, maxDisplayLength); //TAG

                if (inputs.pt1XInput.text.Length == 0) inputs.pt1XInput.text = "0"; //TAG
                if (inputs.pt1YInput.text.Length == 0) inputs.pt1YInput.text = "0";
                //if (inputs.pt1ZInput.text.Length == 0) inputs.pt1ZInput.text = "0"; 
            }
            if (ptSet.ptCoords.ContainsKey("pt2") && inputs.pt2YInput != null)
            {
                inputs.pt2XInput.text = displayText(ptSet.ptCoords["pt2"].X.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt2"].X, maxDisplayLength); //TAG
                inputs.pt2YInput.text = displayText(ptSet.ptCoords["pt2"].Y.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt2"].Y, maxDisplayLength);
                //inputs.pt2ZInput.text = displayText(ptSet.ptCoords["pt2"].Z.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt2"].Z, maxDisplayLength); 

                if (inputs.pt2XInput.text.Length == 0) inputs.pt2XInput.text = "0"; 
                if (inputs.pt2YInput.text.Length == 0) inputs.pt2YInput.text = "0";
                //if (inputs.pt2ZInput.text.Length == 0) inputs.pt2ZInput.text = "0"; //TAG
            }
			
            /* 
            if (ptSet.ptCoords.ContainsKey("pt3") && inputs.pt3XInput != null)
            {
                inputs.pt3XInput.text = displayText(ptSet.ptCoords["pt3"].X.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt3"].X, maxDisplayLength);
                inputs.pt3YInput.text = displayText(ptSet.ptCoords["pt3"].Y.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt3"].Y, maxDisplayLength);
                inputs.pt3ZInput.text = displayText(ptSet.ptCoords["pt3"].Z.tokens, ptInput2D.index, ptInput2D.currExpression == ptSet.ptCoords["pt3"].Z, maxDisplayLength);

                if (inputs.pt3XInput.text.Length == 0) inputs.pt3XInput.text = "0";
                if (inputs.pt3YInput.text.Length == 0) inputs.pt3YInput.text = "0";
                if (inputs.pt3ZInput.text.Length == 0) inputs.pt3ZInput.text = "0";
            }
            */
			//TAG
 
			 
            if (eqnSet.eqnCoefs.ContainsKey("a") && inputs.aInput != null) inputs.aInput.text = displayText(eqnSet.eqnCoefs["a"].tokens, ptInput2D.index, ptInput2D.currExpression == eqnSet.eqnCoefs["a"], maxEqnLength);
            //if (eqnSet.eqnCoefs.ContainsKey("b") && inputs.bInput != null) inputs.bInput.text = displayText(eqnSet.eqnCoefs["b"].tokens, ptInput2D.index, ptInput2D.currExpression == eqnSet.eqnCoefs["b"], maxEqnLength);
            //if (eqnSet.eqnCoefs.ContainsKey("c") && inputs.cInput != null) inputs.cInput.text = displayText(eqnSet.eqnCoefs["c"].tokens, ptInput2D.index, ptInput2D.currExpression == eqnSet.eqnCoefs["c"], maxEqnLength);
            //if (eqnSet.eqnCoefs.ContainsKey("d") && inputs.dInput != null) inputs.dInput.text = displayText(eqnSet.eqnCoefs["d"].tokens, ptInput2D.index, ptInput2D.currExpression == eqnSet.eqnCoefs["d"], maxEqnLength);

            if (inputs.aInput != null && inputs.aInput.text.Length == 0) inputs.aInput.text = "0";
            //if (inputs.bInput != null && inputs.bInput.text.Length == 0) inputs.bInput.text = "0";
            //if (inputs.cInput != null && inputs.cInput.text.Length == 0) inputs.cInput.text = "0";
            //if (inputs.dInput != null && inputs.dInput.text.Length == 0) inputs.dInput.text = "0";

            //if (inputs.bInput != null && inputs.bInput.text[0] != '-') inputs.bInput.text = "+" + inputs.bInput.text;
            //if (inputs.cInput != null && inputs.cInput.text[0] != '-') inputs.cInput.text = "+" + inputs.cInput.text;
            //TAG from lines 341-352
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

