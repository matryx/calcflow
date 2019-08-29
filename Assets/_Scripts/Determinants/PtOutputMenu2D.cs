using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Determinants
{
    public class PtOutputMenu2D : MonoBehaviour
    {

        internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
        {
            PtOutputMenu2D ptoutputMenu2D;
            internal KeyboardInputResponder(PtOutputMenu2D ptoutputMenu2D)
            {
                this.ptoutputMenu2D = ptoutputMenu2D;
            }

            public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
            {
                ptoutputMenu2D.HandleInput(sender.name);
            }

            public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }

        }

        //PRESTREAM CODE
        bool saveable = true;
        bool Saveable
        {
            get
            {
                return saveable;
            }
            set
            {
                if (saveable == false && value == true)
                {
                    saveButton.SetState(0);
                }
                else if (value == false)
                {
                    saveButton.SetState(-1);
                }
                saveable = value;
            }
        }

        FlexActionableComponent saveButton;

        public FlexMenu menu;

        PtManager2D ptManager2D;

        PresentPlane presentPlane;
        public PlaneSolverPointGrab grabber;
        public FlexButtonLockPlane lockButton;

        public void Initialize(PtManager2D pm)
        {
            KeyboardInputResponder responder = new KeyboardInputResponder(this);
            menu.RegisterResponder(responder);

            ptManager2D = pm;
            saveButton = transform.Find("ControlPanel/Save").GetComponent<FlexActionableComponent>();
        }

        protected void HandleInput(string source)
        {
            switch (source)
            {
                default:
                    print("unknown input: " + source);
                    break;
                case "Button_pt1Xinput":	
                    ptManager2D.SetOutput(ptManager2D.ptSet.ptCoords["pt1"].X);
                    break; 
                case "Button_pt1Yinput":
                    ptManager2D.SetOutput(ptManager2D.ptSet.ptCoords["pt1"].Y);
                    break;
                case "Button_pt1Zinput":
                    ptManager2D.SetOutput(ptManager2D.ptSet.ptCoords["pt1"].Z);
                    break; //TAG //X,Y,Z dimensionns changes in the context of 2D
                case "Button_pt2Xinput":
                    ptManager2D.SetOutput(ptManager2D.ptSet.ptCoords["pt2"].X);
                    break; 
                case "Button_pt2Yinput":
                    ptManager2D.SetOutput(ptManager2D.ptSet.ptCoords["pt2"].Y);
                    break;
                case "Button_pt2Zinput":
                    ptManager2D.SetOutput(ptManager2D.ptSet.ptCoords["pt2"].Z);
                    break; //TAG //X,Y,Z dimensionns changes in the context of 2D

				
                case "Button_pt3Xinput":
                    ptManager2D.SetOutput(ptManager2D.ptSet.ptCoords["pt3"].X);
                    break;
                case "Button_pt3Yinput":
                    ptManager2D.SetOutput(ptManager2D.ptSet.ptCoords["pt3"].Y);
                    break;
                case "Button_pt3Zinput":
                    ptManager2D.SetOutput(ptManager2D.ptSet.ptCoords["pt3"].Z);
                    break;
                /* //TAG
				case "Button_a":
                    ptManager.SetOutput(ptManager.eqnSet.eqnCoefs["a"]);
                    break;
                case "Button_b":
                    ptManager.SetOutput(ptManager.eqnSet.eqnCoefs["b"]);
                    break;
                case "Button_c":
                    ptManager.SetOutput(ptManager.eqnSet.eqnCoefs["c"]);
                    break;
                case "Button_d":
                    ptManager.SetOutput(ptManager.eqnSet.eqnCoefs["d"]);
                    break;
				*/
                case "LockPlane":
                    if (grabber.FixedPlane)
                    {
                        grabber.FixedPlane = false;
                        lockButton.LockOff();
                    }
                    else
                    {
                        grabber.FixedPlane = true;
                        lockButton.LockOn();
                    }
                    break;
                case "":
                    break;
            }
            ptManager2D.manageText();
        }

        private void Update()
        {
            //Saveable = !ptManager.paramSurface.isGraphing();
        }
    }
}
