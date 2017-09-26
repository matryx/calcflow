using UnityEngine;
using System.Collections;
using NanoVRController;

public class ControllerAnimation : MonoBehaviour {

    public VRController controller;

    public Transform Ring_r, Ring_g, Ring_b;

    Vector3 targetScale;

    //public bool isLeftGrabbing = false;
    //public bool isRightGrabbing = false;

	// Use this for initialization
	void Start () {
        /*
        Vector3 scale = new Vector3(.5f, .5f, .5f);
        leftRing_b.localScale = scale;
        leftRing_g.localScale = scale;
        leftRing_r.localScale = scale;
        rightRing_b.localScale = scale;
        rightRing_g.localScale = scale;
        rightRing_r.localScale = scale;
        */
        controller.components[ButtonId.GRIP].ComponentPressed += CloseGrabber;
        controller.components[ButtonId.GRIP].ComponentUnpressed += OpenGrabber;


        targetScale = Ring_r.localScale;
	}
	
	// Update is called once per frame
	void Update () {
        Ring_b.localScale = Vector3.Lerp(Ring_b.localScale, targetScale, .5f);
        Ring_g.localScale = Vector3.Lerp(Ring_g.localScale, targetScale, .5f);
        Ring_r.localScale = Vector3.Lerp(Ring_r.localScale, targetScale, .5f);

        //if (controllers.isLeftGrabbing)
        //{
        //    leftRing_b.GetComponent<Renderer>().material = grabbingMat;
        //    leftRing_g.GetComponent<Renderer>().material = grabbingMat;
        //    leftRing_r.GetComponent<Renderer>().material = grabbingMat;
        //}
        //else
        //{
        //    leftRing_b.GetComponent<Renderer>().material = ungrabbingMat;
        //    leftRing_g.GetComponent<Renderer>().material = ungrabbingMat;
        //    leftRing_r.GetComponent<Renderer>().material = ungrabbingMat;
        //}

        //if (controllers.isRightGrabbing)
        //{
        //    rightRing_b.GetComponent<Renderer>().material = grabbingMat;
        //    rightRing_g.GetComponent<Renderer>().material = grabbingMat;
        //    rightRing_r.GetComponent<Renderer>().material = grabbingMat;
        //}
        //else
        //{
        //    rightRing_b.GetComponent<Renderer>().material = ungrabbingMat;
        //    rightRing_g.GetComponent<Renderer>().material = ungrabbingMat;
        //    rightRing_r.GetComponent<Renderer>().material = ungrabbingMat;
        //}
    }

    void CloseGrabber(VRController sender, ControllerComponentArgs e)
    {
        Vector3 scale = new Vector3(.4f, .4f, .4f);
        //leftRing_b.localScale = scale;
        //leftRing_g.localScale = scale;
        //leftRing_r.localScale = scale;

        targetScale = scale;
    }

    void OpenGrabber(VRController sender, ControllerComponentArgs e)
    {
        Vector3 scale = new Vector3(1f, 1f, 1f);
        //rightRing_b.localScale = scale;
        //rightRing_g.localScale = scale;
        //rightRing_r.localScale = scale;
        targetScale = scale;
    }

}
