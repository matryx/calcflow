//using UnityEngine;
//using System.Collections;

//public class FreedrawSceneInteractionSwitch : MonoBehaviour
//{

//    public ControllerEventManager controllers;

//    enum InteractionMode
//    {
//        GRAB,
//        WRITE
//    }
//    InteractionMode iMode;

//    void Start()
//    {
//        controllers.LeftMenuPressed += SwitchMode;
//        controllers.RightMenuPressed += SwitchMode;

//        OnModeSwitch();
//    }

//    void OnEnable()
//    {

//    }

//    void OnDisable()
//    {

//    }

//    void SwitchMode(object sender, ControllerEventArgs e)
//    {
//        if (iMode == InteractionMode.GRAB)
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
//            case InteractionMode.GRAB:
//                if(GetComponent<GrabbableInteraction>())
//                    GetComponent<GrabbableInteraction>().enabled = true;
//                if (GetComponent<GrabAndTypeInteraction>())
//                    GetComponent<GrabAndTypeInteraction>().enabled = true;
//                GetComponent<GraphDraw>().enabled = false;
//                break;
//            case InteractionMode.WRITE:
//                if (GetComponent<GrabbableInteraction>())
//                    GetComponent<GrabbableInteraction>().enabled = false;
//                if (GetComponent<GrabAndTypeInteraction>())
//                    GetComponent<GrabAndTypeInteraction>().enabled = false;
//                GetComponent<GraphDraw>().enabled = true;
//                break;
//        }
//    }
//}
