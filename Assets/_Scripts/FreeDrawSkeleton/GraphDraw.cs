using UnityEngine;
using System.Collections;
using NanoVRController;
using Calcflow.UserStatistics;

public class GraphDraw : MonoBehaviour {
    public VRController leftController;
    public VRController rightController;

    private FreeMarker leftMarker;
    private FreeMarker rightMarker;

    void Awake()
    {
        leftMarker = FreeMarker.addFreeMarker(leftController.transform.Find("Controller (left)").Find("PenTip").gameObject);
        rightMarker = FreeMarker.addFreeMarker(rightController.transform.Find("Controller (right)").Find("PenTip").gameObject);
    }

    void OnEnable()
    {
        StatisticsTracking.StartEvent("Tool Switching", "To Drawing Tool");

        leftController.components[ButtonId.TRIGGER].ComponentPressing  += startDrawing;
        leftController.components[ButtonId.TRIGGER].ComponentUnpressed += stopDrawing;
        leftController.components[ButtonId.GRIP].ComponentPressed      += startErasing;
        leftController.components[ButtonId.GRIP].ComponentPressing     += startErasing;
        leftController.components[ButtonId.GRIP].ComponentUnpressed    += stopErasing;

        rightController.components[ButtonId.TRIGGER].ComponentPressing  += startDrawing;
        rightController.components[ButtonId.TRIGGER].ComponentUnpressed += stopDrawing;
        rightController.components[ButtonId.GRIP].ComponentPressed      += startErasing;
        rightController.components[ButtonId.GRIP].ComponentPressing     += startErasing;
        rightController.components[ButtonId.GRIP].ComponentUnpressed    += stopErasing;

        leftController.transform.Find("Pen").gameObject.SetActive(true);
        rightController.transform.Find("Pen").gameObject.SetActive(true);

        leftController.transform.Find("Controller (right)").Find("PenLabel").gameObject.SetActive(true);
        rightController.transform.Find("Controller (left)").Find("PenLabel").gameObject.SetActive(true);
    }

    void OnDisable()
    {
        StatisticsTracking.EndEvent("Tool Switching", "To Drawing Tool");

        leftController.components[ButtonId.TRIGGER].ComponentPressing += startDrawing;
        leftController.components[ButtonId.TRIGGER].ComponentUnpressed += stopDrawing;
        leftController.components[ButtonId.GRIP].ComponentPressed += startErasing;
        leftController.components[ButtonId.GRIP].ComponentPressing += startErasing;
        leftController.components[ButtonId.GRIP].ComponentUnpressed += stopErasing;

        rightController.components[ButtonId.TRIGGER].ComponentPressing += startDrawing;
        rightController.components[ButtonId.TRIGGER].ComponentUnpressed += stopDrawing;
        rightController.components[ButtonId.GRIP].ComponentPressed += startErasing;
        rightController.components[ButtonId.GRIP].ComponentPressing += startErasing;
        rightController.components[ButtonId.GRIP].ComponentUnpressed += stopErasing;


        leftController.transform.Find("Pen").gameObject.SetActive(false);
        rightController.transform.Find("Pen").gameObject.SetActive(false);

        leftController.transform.Find("Controller (right)").Find("PenLabel").gameObject.SetActive(false);
        rightController.transform.Find("Controller (left)").Find("PenLabel").gameObject.SetActive(false);
    }

    void startDrawing(VRController sender, ControllerComponentArgs e)
    {
        if (sender == leftController)
        {
            leftMarker.startFreeDrawing();
        }
        else
        {
            rightMarker.startFreeDrawing();
        }
    }

    void stopDrawing(VRController sender, ControllerComponentArgs e)
    {
        if (sender == leftController)
        {
            leftMarker.stopFreeDrawing();
        }
        else
        {
            rightMarker.stopFreeDrawing();
        }
    }

    void startErasing(VRController sender, ControllerComponentArgs e)
    {
        if (sender == leftController)
        {
            leftMarker.startFreeErasing();
        }
        else
        {
            rightMarker.startFreeErasing(); 
        }
    }

    void stopErasing(VRController sender, ControllerComponentArgs e)
    {
		if (sender == leftController) 
        {
            leftMarker.stopFreeErasing();
        }
        else
        {
            rightMarker.stopFreeErasing ();
        }
    }
}
