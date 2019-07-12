using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace OLD_Determinants
{
    public class PtOutputMenu : MonoBehaviour
    {

        internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
        {
            PtOutputMenu ptoutputMenu;
            internal KeyboardInputResponder(PtOutputMenu ptoutputMenu)
            {
                this.ptoutputMenu = ptoutputMenu;
            }

            public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
            {
                ptoutputMenu.HandleInput(sender.name);
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

        PtManager ptManager;

        PresentPlane presentPlane;
        //public PlaneSolverPointGrab grabber;
        public FlexButtonLockPlane lockButton;

        public void Initialize(PtManager pm)
        {
            KeyboardInputResponder responder = new KeyboardInputResponder(this);
            menu.RegisterResponder(responder);

            ptManager = pm;
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
                    ptManager.SetOutput(ptManager.ptSet.ptCoords["pt1"].X);
                    break;
                case "Button_pt1Yinput":
                    ptManager.SetOutput(ptManager.ptSet.ptCoords["pt1"].Y);
                    break;
                case "Button_pt1Zinput":
                    ptManager.SetOutput(ptManager.ptSet.ptCoords["pt1"].Z);
                    break;
                case "Button_pt2Xinput":
                    ptManager.SetOutput(ptManager.ptSet.ptCoords["pt2"].X);
                    break;
                case "Button_pt2Yinput":
                    ptManager.SetOutput(ptManager.ptSet.ptCoords["pt2"].Y);
                    break;
                case "Button_pt2Zinput":
                    ptManager.SetOutput(ptManager.ptSet.ptCoords["pt2"].Z);
                    break;
                case "Button_pt3Xinput":
                    ptManager.SetOutput(ptManager.ptSet.ptCoords["pt3"].X);
                    break;
                case "Button_pt3Yinput":
                    ptManager.SetOutput(ptManager.ptSet.ptCoords["pt3"].Y);
                    break;
                case "Button_pt3Zinput":
                    ptManager.SetOutput(ptManager.ptSet.ptCoords["pt3"].Z);
                    break;
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
                /*case "LockPlane":
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
                    break;*/
                case "":
                    break;
            }
            ptManager.manageText();
        }

        private void Update()
        {
            //Saveable = !ptManager.paramSurface.isGraphing();
        }
    }
}
