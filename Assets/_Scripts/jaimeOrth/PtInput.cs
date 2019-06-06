using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace orthProj
{
    public class PtInput : MonoBehaviour
    {
        public GameObject lineCover;
        public GameObject lineButtonCover;
        public GameObject planeButtonCover;

        internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
        {
            PtInput ptInput;
            internal KeyboardInputResponder(PtInput ptInput)
            {
                this.ptInput = ptInput;
            }

            public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
            {
                ptInput.HandleInput(sender.name);
            }

            public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }

        }

        [HideInInspector]
        public CalcOutput currExpression;
        [HideInInspector]
        public int index = 0;
        [HideInInspector]

        private PtManager ptManager;

        private FlexMenu keyboard;

        KeyboardInputResponder responder;

        public void ChangeOutput(CalcOutput calcOutput)
        {
            currExpression = calcOutput;
            index = currExpression.tokens.Count;
        }

        public void setPlane()
        {
            lineCover.SetActive(false);
            planeButtonCover.SetActive(false); //swapped grey out
            lineButtonCover.SetActive(true); //swapped grey out
        }

        public void setLine()
        {
            lineCover.SetActive(true);
            planeButtonCover.SetActive(true); //swapped grey out
            lineButtonCover.SetActive(false); //swapped grey out
        }

        public void HandleInput(string buttonID)
        {
            print(buttonID + " fired");
            #region switch
            switch (buttonID)
            {
                default:
                    currExpression.tokens.Insert(index, buttonID);
                    index++;
                    ptManager.inputReceived = true;
                    break;
                //case "xShortCut" || "yShortCut":
                //    shortCutManager(buttonID);
                //    break;
                case "yShortCut":  /// TODO:  WHY???
                    Debug.Log("xSHORT CUT FIRED!!!!!!!!!!!!!!");
                    ptManager.updatePoint("pt2", new Vector3(1, 0, 0), false); // first axis
                    setLine();
                    ptManager.inputReceived = true;
                    break;
                case "xShortCut":  /// TODO:  WHY???
                    Debug.Log("ySHORT CUT FIRED!!!!!!!!!!!!!!");
                    ptManager.updatePoint("pt2", new Vector3(0, 1, 0), false); // first axis
                    setLine();
                    ptManager.inputReceived = true;
                    break;
                case "zShortCut":
                    Debug.Log("zSHORT CUT FIRED!!!!!!!!!!!!!!");
                    ptManager.updatePoint("pt2", new Vector3(0, 0, 1), false); // first axis
                    setLine();
                    ptManager.inputReceived = true;
                    break;
                case "xyShortCut":
                    ptManager.updatePoint("pt2", new Vector3(1, 0, 0), false); // first axis
                    setPlane();
                    ptManager.updatePoint("pt3", new Vector3(0, 1, 0), false); // first axi
                    ptManager.inputReceived = true;
                    break;
                case "yzShortCut":  /// TODO:  WHY???
                    ptManager.updatePoint("pt2", new Vector3(1, 0, 0), false); // first axis
                    setPlane();
                    ptManager.updatePoint("pt3", new Vector3(0, 0, 1), false); // first axis
                    ptManager.inputReceived = true;
                    break;
                case "xzShortCut":  /// TODO:  WHY???
                    ptManager.updatePoint("pt2", new Vector3(0, 1, 0), false); // first axis
                    setPlane();
                    ptManager.updatePoint("pt3", new Vector3(0, 0, 1), false); // first axis
                    ptManager.inputReceived = true;
                    //  presentline.
                    break;
                case "ProjPlane":
                    Debug.Log("PROJPLANE FIRED!!!!!!!!!!!!!!");
                    ptManager.updatePoint("pt2", new Vector3(1, 1, 1), false); // first axis
                    setPlane();

                    //here the plane is not resetting, the point is internally staying there probably and the not actually updating the plane
                    //call plane or disable plane?
                    //also the line's center point becomes odd.
                    ptManager.inputReceived = true;
                    break;
                case "ProjLine":
                    Debug.Log("LINE FIRED!!!!!!!!!!!!!!");
                    ptManager.updatePoint("pt2", new Vector3(1, 1, 1), false); // first axis
                    //lineCover.GetComponent<MeshRenderer>().enabled = false;
                    setLine();
                    ptManager.inputReceived = true;
                    break;
                case "Paste":
                    /*
                    string temp = GUIUtility.systemCopyBuffer;
                    List<string> tempList = ExpressionParser.Parse(temp);
                    currExpression.tokens.InsertRange(index, tempList);
                    index += tempList.Count;
                    calcManager.inputReceived = true;
                    */
                    break;
                #region control_buttons
                case "Button_del":
                    if (index > 0)
                    {
                        currExpression.tokens.RemoveAt(index - 1);
                        index--;
                        ptManager.inputReceived = true;
                    }
                    break;
                case "Button_Clear":
                    index = 0;
                    currExpression.tokens.Clear();
                    ptManager.inputReceived = true;
                    break;
                case "Button_Enter":
                    ptManager.inputReceived = true;
                    break;
                case "Button_left":
                    index--;
                    if (index < 0) index = 0;
                    break;
                case "Button_right":
                    index++;
                    if (index > currExpression.tokens.Count) index = currExpression.tokens.Count;
                    break;
                case "Button_start":
                    index = 0;
                    break;
                case "Button_end":
                    index = currExpression.tokens.Count;
                    break;
                    #endregion
            }
            #endregion
            ptManager.updateText = true;
        }

        public void Initialize(PtManager pm)
        {
            ptManager = pm;
            keyboard = GetComponent<FlexMenu>();
            responder = new KeyboardInputResponder(this);
            keyboard.RegisterResponder(responder);
        }

        public void RewriteInput(float newValue)
        {
            index = 0;
            currExpression.tokens.Clear();
            string s = roundString(newValue, "{0:0.00}");
            if (ptManager.eqnInput) s = roundString(newValue, "{0:0.0}");
            foreach (char c in s)
            {
                currExpression.tokens.Insert(index, c.ToString());
                index++;
            }
        }

        public void RewriteInput()
        {
            index = 0;
            currExpression.tokens.Clear();
        }

        public string roundString(float input, string format)
        {
            string a = input.ToString();
            string b = string.Format(format, input);
            if (a.Length <= b.Length)
            {
                return a;
            }
            else
            {
                return b;
            }
        }
    }
}
