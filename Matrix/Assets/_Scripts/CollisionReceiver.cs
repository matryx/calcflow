using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CollisionReceiver : MonoBehaviour {
    public delegate void CollisionCallback(Collision collision);

    public event CollisionCallback CollisionStart;
    public event CollisionCallback CollisionStay;
    public event CollisionCallback CollisionEnd;

    public void OnColiisionEnter(Collision collision)
    {
        CollisionStart.Invoke(collision);
    }

    public void OnColiisionStay(Collision collision)
    {
        CollisionStay.Invoke(collision);
    }

    public void OnCollisionExit(Collision collision)
    {
        CollisionEnd.Invoke(collision);
    }

}

