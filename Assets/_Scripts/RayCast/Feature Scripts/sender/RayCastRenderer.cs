using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(RayCastSender))]

public class RayCastRenderer : MonoBehaviour {

    GameObject laser;
    LineRenderer laserLine;
    public Material laserMat;
    RayCastSender rayCastSender;

    // Use this for initialization
    void Start () {
        rayCastSender = GetComponent<RayCastSender>();
        InitLaser();
	}
	
	// Update is called once per frame
	void Update () {
        updateLaser();
	}


    private void InitLaser()
    {
        laser = new GameObject();
        laser.transform.parent = this.transform;
        laser.name = "laser";
        laserLine = laser.AddComponent<LineRenderer>();
        laserLine.GetComponent<Renderer>().material = laserMat;
        laserLine.positionCount = 2;
        laserLine.startWidth = .01f;
        laserLine.endWidth = .01f;
        laserLine.SetPosition(0, Vector3.zero);
        laserLine.SetPosition(1, Vector3.zero);
    }

    void updateLaser()
    {
        if (!rayCastSender.enabled)
        {
            laser.SetActive(false);
            return;
        }


        laser.SetActive(true);

        laserLine.SetPosition(0, transform.position);
        if (rayCastSender.CurrTargetData != null && rayCastSender.CurrTargetData.hitting)
        {
            laserLine.SetPosition(1, rayCastSender.TargetPoint);
        }
        else
        {
            laserLine.SetPosition(1, transform.forward * 1000);
        }
    }
}
