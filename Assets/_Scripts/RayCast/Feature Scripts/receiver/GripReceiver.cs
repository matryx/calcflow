using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class GripReceiver : MonoBehaviour {

    public delegate void GripCallBack(Grabber grabber);

    public event GripCallBack GrabReceivedEvent;

    public void SendGripMessage(Grabber grabber)
    {
        if (GrabReceivedEvent != null)
        {
            GrabReceivedEvent.Invoke(grabber);
        }
    }


}
