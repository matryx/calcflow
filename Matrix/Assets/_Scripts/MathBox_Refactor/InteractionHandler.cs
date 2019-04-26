using UnityEngine;
using System;
using System.Collections;
using System.Collections.Generic;

public class InteractionHandler : MonoBehaviour, FlexMenu.FlexMenuResponder {

    public enum InteractionMode:int
    {
        CREATE = 0,
        MODIFY,
        ADDITION,
        CROSSPRODUCT
    }
    public InteractionMode iMode = InteractionMode.CREATE;
    //InteractionMode prevMode = InteractionMode.CREATE;

    public MathBox_Vector vectorPrefab;
    //public Plotter3D surfacePrefab;

    public GameObject cartesian, spherical, cylindrical;

    public ControllerEventManager controllers;
    public FlexMenu leftMenu;

    public enum CoordSystemType : int {
        CARTESIAN = 0,
        SPHERICAL,
        CYLINDRICAL
    }
    public CoordSystemType coordType = CoordSystemType.CARTESIAN;

    public Transform coordSystem;
    public Transform origin;

    List<MathBox_Vector> vector_list;
    List<MathBox_Vector> usercreated_list;
    List<MathBox_Vector> selected_vectors;

    MathBox_Vector leftActiveVec, rightActiveVec;

    bool isLeftGripped = false, isRightGripped = false;
    Vector3 leftControllerPos, rightControllerPos;

    bool leftMenuPress = false;

    //Plotter3D surface;

    void SetInteractionMode(InteractionMode e)
    {
        iMode = e;
        OnModeSwitch();
    }

    void SwitchCoordSystem()
    {
        int curr = (int)coordType;
        curr++;
        curr = curr % 3;
        if (Enum.IsDefined(typeof(CoordSystemType), curr))
        {
            coordType = (CoordSystemType)curr;
        }
        switch (coordType)
        {
            case CoordSystemType.CARTESIAN:
                cartesian.SetActive(true);
                spherical.SetActive(false);
                cylindrical.SetActive(false);
                break;
            case CoordSystemType.CYLINDRICAL:
                cartesian.SetActive(false);
                spherical.SetActive(false);
                cylindrical.SetActive(true);
                break;
            case CoordSystemType.SPHERICAL:
                cartesian.SetActive(false);
                spherical.SetActive(true);
                cylindrical.SetActive(false);
                break;
        }
    }

	// Use this for initialization
	void Start () {
        spherical.SetActive(false);
        cylindrical.SetActive(false);

        leftMenu.RegisterResponder(this);

        vector_list = new List<MathBox_Vector>();
        usercreated_list = new List<MathBox_Vector>();
        selected_vectors = new List<MathBox_Vector>();

        controllers.LeftPadTouched += OpenLeftSelector;
        controllers.LeftPadTouching += UpdateLeftSelector;
        controllers.LeftPadUntouched += CloseLeftSelector;
        controllers.LeftPadPressed += SelectOnLeftMenu;

        controllers.LeftTriggerPressed += CreateVector;
        controllers.LeftTriggerHolding += DragVector;
        controllers.LeftTriggerUnpressed += FinalizeVector;
        controllers.RightTriggerPressed += CreateVector;
        controllers.RightTriggerHolding += DragVector;
        controllers.RightTriggerUnpressed += FinalizeVector;
    }
	
	// Update is called once per frame
	void Update () {
        
	}

    void OnModeSwitch()
    {
        controllers.ClearTriggerEvents();

        switch (iMode)
        {
            case InteractionMode.CREATE:
                controllers.LeftTriggerPressed += CreateVector;
                controllers.LeftTriggerHolding += DragVector;
                controllers.LeftTriggerUnpressed += FinalizeVector;
                controllers.RightTriggerPressed += CreateVector;
                controllers.RightTriggerHolding += DragVector;
                controllers.RightTriggerUnpressed += FinalizeVector;
                break;
            case InteractionMode.MODIFY:
                controllers.LeftTriggerPressed += SelectVector;
                controllers.LeftTriggerHolding += DragVector;
                controllers.LeftTriggerUnpressed += FinishEditing;
                controllers.RightTriggerPressed += SelectVector;
                controllers.RightTriggerHolding += DragVector;
                controllers.RightTriggerUnpressed += FinishEditing;
                break;
            case InteractionMode.ADDITION:
                controllers.LeftTriggerPressed += AdditionOrSelect;
                controllers.RightTriggerPressed += AdditionOrSelect;
                break;
            case InteractionMode.CROSSPRODUCT:
                controllers.LeftTriggerPressed += CrossOrSelect;
                controllers.RightTriggerPressed += CrossOrSelect;
                break;
        }
    }

    void OpenLeftSelector(object sender, ControllerEventArgs e)
    {
        //leftMenu.EnableSelector(e.padX * 0.045f, e.padY * 0.045f);
    }
    void UpdateLeftSelector(object sender, ControllerEventArgs e)
    {
        //leftMenu.UpdateSelector(e.padX * 0.045f, e.padY * 0.045f);
    }
    void CloseLeftSelector(object sender, ControllerEventArgs e)
    {
        //leftMenu.DisableSelector();
    }
    void SelectOnLeftMenu(object sender, ControllerEventArgs e)
    {
        leftMenuPress = true;
    }

    void CreateVector(object sender, ControllerEventArgs e)
    {
        Transform tip = e.controller.GetComponentInChildren<InitAttachment>().tip;
        MathBox_Vector vector = Instantiate(vectorPrefab);
        vector.transform.SetParent(coordSystem, false);
        vector_list.Add(vector);
        usercreated_list.Add(vector);
        vector.SetPosition(origin.position, tip.position);
        if (e.isLeft)
        {
            leftActiveVec = vector;
        }
        else
        {
            rightActiveVec = vector;
        }
    }

    void AdditionOrSelect(object sender, ControllerEventArgs e)
    {
        Transform tip = e.controller.GetComponentInChildren<InitAttachment>().tip;
        if(selected_vectors.Count == 2)
        {
            AddSelected();
        }
        else
        {
            foreach (MathBox_Vector v in usercreated_list)
            {
                if ((v.GetEnd - tip.position).magnitude < 1)
                {
                    ToggleSelection(v);
                }
            }
        }
    }
    void CrossOrSelect(object sender, ControllerEventArgs e)
    {
        Transform tip = e.controller.GetComponentInChildren<InitAttachment>().tip;
        if (selected_vectors.Count == 2)
        {
            CrossSelected();
        }
        else
        {
            foreach (MathBox_Vector v in usercreated_list)
            {
                if ((v.GetEnd - tip.position).magnitude < 1)
                {
                    ToggleSelection(v);
                }
            }
        }
    }

    MathBox_Vector LastSelection()
    {
        if (selected_vectors.Count != 0)
        {
            MathBox_Vector last = selected_vectors[selected_vectors.Count - 1];
            selected_vectors.RemoveAt(selected_vectors.Count - 1);
            last.select();
            return last;
        }
        else
        {
            return null;
        }
    }

    public void AddSelected()
    {
        MathBox_Vector v1, v2;
        if ((v2 = LastSelection()) != null)
        {
            if ((v1 = LastSelection()) != null)
            {
                VectorAddition(v1, v2);
            }
        }
    }

    public void CrossSelected()
    {
        MathBox_Vector v1, v2;
        if ((v2 = LastSelection()) != null)
        {
            if ((v1 = LastSelection()) != null)
            {
                VectorCross(v1, v2);
            }
        }
    }
    void VectorAddition(MathBox_Vector v1, MathBox_Vector v2)
    {
        //operand1
        Vector3 start = v1.vectorInfo;
        MathBox_Vector vector = Instantiate(vectorPrefab);
        vector.SetVectorType(MathBox_Vector.VectorType.Addition_operand1, v1, v2);
        vector.transform.SetParent(coordSystem, false);
        vector_list.Add(vector);

        //operand2
        start = v2.vectorInfo;
        vector = Instantiate(vectorPrefab);
        vector.SetVectorType(MathBox_Vector.VectorType.Addition_operand2, v1, v2);
        vector.transform.SetParent(coordSystem, false);
        vector_list.Add(vector);

        //result
        start = origin.position;
        vector = Instantiate(vectorPrefab);
        vector.SetVectorType(MathBox_Vector.VectorType.Addition, v1, v2);
        vector.transform.SetParent(coordSystem, false);
        vector_list.Add(vector);
    }

    void VectorCross(MathBox_Vector v1, MathBox_Vector v2)
    {
        Vector3 start = origin.position;
        //Vector3 end = Vector3.Cross(v1.vectorInfo, v2.vectorInfo);
        MathBox_Vector vector = Instantiate(vectorPrefab);
        vector.SetVectorType(MathBox_Vector.VectorType.CrossProduct, v1, v2);
        vector.transform.SetParent(coordSystem, false);
        vector_list.Add(vector);
    }

    void ToggleSelection(MathBox_Vector selected)
    {
        selected.select();
        if (selected_vectors.Contains(selected))
        {
            selected_vectors.Remove(selected);
        }
        else
        {
            selected_vectors.Add(selected);
        }
    }

    void ClearScene()
    {
        foreach (MathBox_Vector v in vector_list)
        {
            v.ClearVector();
        }
        vector_list.Clear();
        usercreated_list.Clear();
        selected_vectors.Clear();
    }

    void SelectVector(object sender, ControllerEventArgs e)
    {
        Transform tip = e.controller.GetComponent<InitAttachment>().tip;
        foreach(MathBox_Vector v in usercreated_list)
        {
            if((v.GetEnd - tip.position).magnitude < 1)
            {
                if (e.isLeft)
                {
                    leftActiveVec = v;
                }
                else
                {
                    rightActiveVec = v;
                }
            }
        }
    }

    void DragVector(object sender, ControllerEventArgs e)
    {
        Transform tip = e.controller.GetComponentInChildren<InitAttachment>().tip;
        if (e.isLeft)
        {
            if(leftActiveVec == null)
            {
                return;
            }
            leftActiveVec.SetPosition(origin.position, tip.position);
        }
        else
        {
            if (rightActiveVec == null)
            {
                return;
            }
            rightActiveVec.SetPosition(origin.position, tip.position);
        }
    }

    void FinalizeVector(object sender, ControllerEventArgs e)
    {
        if(coordType == CoordSystemType.CARTESIAN)
        {
            if (e.isLeft)
            {
                leftActiveVec.CreateInCartesian();
                leftActiveVec = null;
            }
            else
            {
                rightActiveVec.CreateInCartesian();
                rightActiveVec = null;
            }
        }
        else
        {
            if (e.isLeft)
            {
                leftActiveVec.CreateInGeneral();
                leftActiveVec = null;
            }
            else
            {
                rightActiveVec.CreateInGeneral();
                rightActiveVec = null;
            }
        }
    }

    void FinishEditing(object sender, ControllerEventArgs e)
    {
        if(e.isLeft && leftActiveVec == null)
        {
            return;
        }
        if(!e.isLeft && rightActiveVec == null)
        {
            return;
        }
        if (coordType == CoordSystemType.CARTESIAN)
        {
            if (e.isLeft)
            {
                leftActiveVec.FinishEditInCartesian();
                leftActiveVec = null;
            }
            else
            {
                rightActiveVec.FinishEditInCartesian();
                rightActiveVec = null;
            }
        }
        else
        {
            if (e.isLeft)
            {
                leftActiveVec.FinishEditInGeneral();
                leftActiveVec = null;
            }
            else
            {
                rightActiveVec.FinishEditInGeneral();
                rightActiveVec = null;
            }
        }
    }

    /***************************************************************
    * Touchpad Menu
    ***************************************************************/
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
        if (leftMenuPress)
        {
            switch (sender.name)
            {
                case "Create":
                    SetInteractionMode(InteractionMode.CREATE);
                    break;
                case "Edit":
                    SetInteractionMode(InteractionMode.MODIFY);
                    break;
                case "Cross":
                    SetInteractionMode(InteractionMode.CROSSPRODUCT);
                    break;
                case "Addition":
                    SetInteractionMode(InteractionMode.ADDITION); 
                    break;
                case "Clear":
                    ClearScene();
                    break;
                case "Switch":
                    SwitchCoordSystem();
                    break;
                case "Plot":
                    
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
        //if ((leftMenuPress || sender.State == 2) && ((FlexButtonComponent)sender).buttonType == FlexButtonComponent.ButtonType.SELECTION)
        //{
        //    sender.ParentPanel.ClearSelection(true);
        //    sender.SetState(2);
        //}
        //else
        //{
        //    sender.SetState(1);
        //}
        //leftMenuPress = false;
    }
    public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider)
    {
        if (sender.State != 2)
            sender.SetState(0);
    }
}
