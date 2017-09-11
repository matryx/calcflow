using UnityEngine;
using System.Collections;

public class ElectricFieldInteraction : MonoBehaviour {

    public ControllerEventManager controllers;
    public ElectricField electricField;

	// Use this for initialization
	void Start () {
        controllers.LeftTriggerPressed += AddPositiveCharge;
        controllers.RightTriggerPressed += AddNegativeCharge;

        controllers.LeftMenuPressed += ClearScene;
        controllers.RightMenuPressed += ClearScene;

        controllers.LeftPadPressed += MoveDivergenceCube;
        controllers.RightPadPressed += MoveDivergenceCube;
	}
	
	// Update is called once per frame
	void Update () {
	
	}

    void AddPositiveCharge(object sender, ControllerEventArgs e)
    {
        Transform tip = e.controller.GetComponentInChildren<InitAttachment>().tip;
        electricField.AddCharge(tip.position, 2f);
    }

    void AddNegativeCharge(object sender, ControllerEventArgs e)
    {
        Transform tip = e.controller.GetComponentInChildren<InitAttachment>().tip;
        electricField.AddCharge(tip.position, -2f);
    }

    void ClearScene(object sender, ControllerEventArgs e)
    {
        electricField.Clear();
    }

    void MoveDivergenceCube(object sender, ControllerEventArgs e)
    {
        Transform tip = e.controller.GetComponentInChildren<InitAttachment>().tip;
        electricField.MoveDivergenceBox(tip.position);
    }
}
