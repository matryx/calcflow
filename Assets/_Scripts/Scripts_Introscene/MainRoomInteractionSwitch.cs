//using UnityEngine;
//using System.Collections;

//public class MainRoomInteractionSwitch : MonoBehaviour {

//    public ControllerEventManager controllers;

//    enum InteractionMode
//    {
//        GRAB,
//        WRITE
//    }
//    InteractionMode iMode;

//    void Start()
//    {
//        controllers.LeftPadPressed += SwitchMode;
//		controllers.RightPadPressed += SwitchMode;

//		OnModeSwitch ();
//    }

//    void OnEnable()
//    {
        
//    }

//    void OnDisable()
//    {

//    }

//    void SwitchMode(object sender, ControllerEventArgs e)
//    {
//        if(iMode == InteractionMode.GRAB)
//        {
//            iMode = InteractionMode.WRITE;
//        }
//        else
//        {
//            iMode = InteractionMode.GRAB;
//        }
//        OnModeSwitch();
//    }

//    void OnModeSwitch()
//    {
//        switch (iMode)
//        {
//			case InteractionMode.GRAB:
//				GetComponent<HandAnimation> ().enabled = true;
//				GetComponent<RayWhiteBoard> ().enabled = false;
//                break;
//			case InteractionMode.WRITE:
//				GetComponent<HandAnimation> ().enabled = false;	
//				GetComponent<RayWhiteBoard> ().enabled = true;

//                break;
//        }
//    }
//}
