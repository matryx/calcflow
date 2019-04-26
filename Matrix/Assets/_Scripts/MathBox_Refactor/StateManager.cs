using UnityEngine;
using System.Collections;
using System;
using System.Linq;

public class StateManager : MonoBehaviour, FlexMenu.FlexMenuResponder {

    //public GameObject munePrefab;
    public FlexMenu leftMenu;
    public FlexMenu rightMenu;

    public ControllerInteraction controllerManager;
    public CoordinateSystemInteraction scene;

    private bool leftPress = false, rightPress = false;

    public void SetRightMenuPress(bool a)
    {
        rightPress = a;
    }
    public void SetLeftMenuPress(bool a)
    {
        leftPress = a;
    }

    public FlexMenu OpenRightMenu(Transform attach = null, String panelName = "")
    {
        if (attach)
        {
            rightMenu.transform.SetParent(attach, false);
        }
        rightMenu.transform.localPosition = new Vector3(0f, 0.025f, -0.0693f);
        rightMenu.transform.eulerAngles = new Vector3(-13f, 0f, 0f);
        //rightMenu.OpenMenu(panelName, false);
        rightMenu.OpenPanel(panelName);
        rightMenu.transform.Find("MainPanel").Find("Create").GetComponent<FlexActionableComponent>().SetState(2);
        return rightMenu;
    }
    public FlexMenu OpenLeftMenu(Transform attach = null, String panelName = "")
    {
        if (attach)
        {
            leftMenu.transform.SetParent(attach, false);
        }
        leftMenu.transform.localPosition = new Vector3(0f, 0.025f, -0.0693f);
        leftMenu.transform.eulerAngles = new Vector3(-13f, 0f, 0f);
        //rightMenu.OpenMenu(panelName, false);
        leftMenu.ResetPanels();
        leftMenu.AddAllChildPanels();
        leftMenu.OpenPanel(panelName);
        leftMenu.transform.Find("MainPanel").Find("Create").GetComponent<FlexActionableComponent>().SetState(2);
        return leftMenu;
    }

    public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
    {
        if (rightPress)
        {
            switch (sender.name)
            {
                case "Create":
                    scene.SetInteractionMode(CoordinateSystemInteraction.InteractionMode.Create);
                    break;
                case "Edit":
                    scene.SetInteractionMode(CoordinateSystemInteraction.InteractionMode.Modify);
                    break;
                case "Cross":
                    scene.SetInteractionMode(CoordinateSystemInteraction.InteractionMode.Operate);
                    scene.SetOpMode(CoordinateSystemInteraction.OperateMode.CrossProduct);
                    break;
                case "Addition":
                    scene.SetInteractionMode(CoordinateSystemInteraction.InteractionMode.Operate);
                    scene.SetOpMode(CoordinateSystemInteraction.OperateMode.Addition);
                    break;
                case "Clear":
                    scene.ClearScene();
                    scene.ClearSurface();
                    break;
                case "Switch":
                    scene.SwitchCoordSystem();
                    break;
                case "Plot":
                    scene.CreateSurface();
                    break;
            }
            //rightPress = false;
        }
        else if (leftPress)
        {
            //ApplyLeftAppMode();
            switch (sender.name)
            {
                case "Create":
                    scene.SetInteractionMode(CoordinateSystemInteraction.InteractionMode.Create);
                    break;
                case "Edit":
                    scene.SetInteractionMode(CoordinateSystemInteraction.InteractionMode.Modify);
                    break;
                case "Cross":
                    scene.SetInteractionMode(CoordinateSystemInteraction.InteractionMode.Operate);
                    scene.SetOpMode(CoordinateSystemInteraction.OperateMode.CrossProduct);
                    break;
                case "Addition":
                    scene.SetInteractionMode(CoordinateSystemInteraction.InteractionMode.Operate);
                    scene.SetOpMode(CoordinateSystemInteraction.OperateMode.Addition);
                    break;
                case "Clear":
                    scene.ClearScene();
                    scene.ClearSurface();
                    break;
                case "Switch":
                    scene.SwitchCoordSystem();
                    break;
                case "Plot":
                    scene.CreateSurface();
                    break;
            }
            //leftPress = false;
        }
        //if (((interfaceManager.GetLeftHIDActionPressDown() && leftMenu.name == sender.ParentPanel.GetMenuName()) ||
        //	(interfaceManager.GetRightHIDActionPressDown() && rightMenu.name == sender.ParentPanel.GetMenuName()) ||
        //	sender.State == 2) && ((FlexButtonComponent)sender).buttonType == FlexButtonComponent.ButtonType.SELECTION) 

        //if (((leftPress && leftMenu.name == sender.ParentPanel.GetMenuName()) ||
        //    (rightPress && rightMenu.name == sender.ParentPanel.GetMenuName()) ||
        //    sender.State == 2))
        // look for NGInterface_GetRightHIDActionPressDown in NGHTCViveInterface if this is confusing
        //if ((leftPress || rightPress || sender.State == 2) && ((FlexButtonComponent)sender).buttonType == FlexButtonComponent.ButtonType.SELECTION)
        //{
        //    sender.ParentPanel.ClearSelection(true);
        //    sender.SetState(2);
        //}
        //else
        //{
        //    sender.SetState(1);
        //}

        leftPress = rightPress = false;
    }

    public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider)
    {
        if (sender.State != 2)
            sender.SetState(0);
    }

    // Use this for initialization
    void Start () {
        rightMenu.RegisterResponder(this);
        leftMenu.RegisterResponder(this);
	}
	
	// Update is called once per frame
	void Update () {
	
	}
}
