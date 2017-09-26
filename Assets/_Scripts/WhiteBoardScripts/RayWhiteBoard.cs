using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class RayWhiteBoard : MonoBehaviour {
	public ControllerEventManager controllers;

	private Marker leftMarker;
	private Marker rightMarker;

	// Use this for initialization
	void Awake () {
		leftMarker = Marker.addMarker (controllers.transform.Find ("Controller (left)").Find ("PenTip").gameObject);
		rightMarker = Marker.addMarker (controllers.transform.Find ("Controller (right)").Find ("PenTip").gameObject);
	}

	void OnEnable() {
		leftMarker.setMarkerActive(true);
		rightMarker.setMarkerActive(true);

		controllers.LeftGripPressed += markerClick;
		controllers.LeftGripHolding += startDrawing;
		controllers.LeftGripUnpressed += stopDrawing;
		controllers.LeftTriggerPressed += startErasing;
		controllers.LeftTriggerHolding += startErasing;
		controllers.LeftTriggerUnpressed += stopErasing;

		controllers.RightGripPressed += markerClick;
		controllers.RightGripHolding += startDrawing;
		controllers.RightGripUnpressed += stopDrawing;
		controllers.RightTriggerPressed += startErasing;
		controllers.RightTriggerHolding += startErasing;
		controllers.RightTriggerUnpressed += stopErasing;

        controllers.GetComponent<SteamVR_ControllerManager>().left.transform.Find("Pen").gameObject.SetActive(true);
        controllers.GetComponent<SteamVR_ControllerManager>().right.transform.Find("Pen").gameObject.SetActive(true);
    }

	void OnDisable() {
		controllers.LeftGripPressed -= markerClick;
		controllers.LeftGripHolding -= startDrawing;
		controllers.LeftGripUnpressed -= stopDrawing;
		controllers.LeftTriggerPressed -= startErasing;
		controllers.LeftTriggerHolding -= startErasing;
		controllers.LeftTriggerUnpressed -= stopErasing;

		controllers.RightGripPressed -= markerClick;
		controllers.RightGripHolding -= startDrawing;
		controllers.RightGripUnpressed -= stopDrawing;
		controllers.RightTriggerPressed -= startErasing;
		controllers.RightTriggerHolding -= startErasing;
		controllers.RightTriggerUnpressed -= stopErasing;

        controllers.GetComponent<SteamVR_ControllerManager>().left.transform.Find("Pen").gameObject.SetActive(false);
        controllers.GetComponent<SteamVR_ControllerManager>().right.transform.Find("Pen").gameObject.SetActive(false);

        leftMarker.setMarkerActive(false);
		rightMarker.setMarkerActive(false);
	}

	void startErasing(object sender, ControllerEventArgs e){
		if (e.isLeft) {
			leftMarker.startErasing();
		} else {
			rightMarker.startErasing();
		}
	}


	void startDrawing(object sender, ControllerEventArgs e){
		if (e.isLeft) {
			leftMarker.startDrawing();
		} else {
			rightMarker.startDrawing();
		}
	}

	void stopDrawing(object sender, ControllerEventArgs e){
		if (e.isLeft) {
			leftMarker.stopDrawing();
		} else {
			rightMarker.stopDrawing();
		}
	}

	void stopErasing(object sender, ControllerEventArgs e){
		if (e.isLeft) {
			leftMarker.stopErasing();
		} else {
			rightMarker.stopErasing ();
		}
	}
		
	void markerClick (object sender, ControllerEventArgs e){
		Marker marker;
		RaycastHit hit;
		if (e.isLeft) {
			marker = leftMarker;
		} else {
			marker = rightMarker;
		}

        if (!(marker.penTarget (out hit))) return;

		switch (hit.transform.name) {
		case ("Minimize"):
			StartCoroutine (marker.getBoard ().Collapse ());
			break;
		case ("Maximize"):
            StartCoroutine(marker.getBoard ().Expand ());
			break;
		case ("Delete"):
            //want to switch to grab mode after deleting a board
			marker.getBoard().deleteBoard ();
            marker.stopDrawing();
			break;
		case ("ScreenShot"):
			//marker.getBoard ().print ();
			break;
		case ("Rotate"):
			marker.getBoard ().snapBoard ();
			break;
		case ("Undo"):
			marker.getBoard ().undoAction ();
			break; 
		case ("Redo"):
			marker.getBoard ().redoAction ();
			break;
        default:
			startDrawing (sender, e);
			break;
		}
	}
}
