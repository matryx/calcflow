using UnityEngine;
using System.Collections;

public class Oculus_3DDraw : MonoBehaviour {

    Oculus_TrackedController controller;
    Oculus_HandAnim anim;
    private FreeMarker marker;

    void Awake()
    {
        controller = GetComponent<Oculus_TrackedController>();
        anim = GetComponent<Oculus_HandAnim>();
        //if (controller.handedness == OvrTouch.Controllers.HandednessId.Left)
            //marker = FreeMarker.addFreeMarker(transform.Find("l_hand_skeletal").Find("hands:l_hand_world").Find("hands:b_l_hand").Find("hands:b_l_index1").Find("hands:b_l_index2").Find("hands:b_l_index3").Find("hands:b_l_index_ignore").gameObject);
            //marker = FreeMarker.addFreeMarker(transform.Find("Pentip").gameObject);
        //else
            //marker = FreeMarker.addFreeMarker(transform.Find("r_hand_skeletal").Find("hands:r_hand_world").Find("hands:b_r_hand").Find("hands:b_r_index1").Find("hands:b_r_index2").Find("hands:b_r_index3").Find("hands:b_r_index_ignore").gameObject);
            //marker = FreeMarker.addFreeMarker(transform.Find("Pentip").gameObject);
    }

    void OnEnable()
    {
        controller.indexTriggerPressed += StartDrawing;
        controller.indexTriggerUnpress += EndDrawing;
        controller.handTriggerPressed += StartErasing;
        controller.handTriggerUnpressing += EndErasing;
        anim.SetFlex(0.15f);
        anim.SetPoint(false);
        anim.SetThumb(false);
        anim.SetPose(0);
        transform.Find("Pen").gameObject.SetActive(true);
    }

    void OnDisable()
    {
        controller.indexTriggerPressed -= StartDrawing;
        controller.indexTriggerUnpress -= EndDrawing;
        controller.handTriggerPressed -= StartErasing;
        controller.handTriggerUnpressing -= EndErasing;
        transform.Find("Pen").gameObject.SetActive(false);
    }

    void StartDrawing(object sender, Oculus_ControllerArgs e)
    {
        marker.startFreeDrawing();
    }
    void EndDrawing(object sender, Oculus_ControllerArgs e)
    {
        if (e.axis_1d < 0.5f)
            marker.stopFreeDrawing();
    }
    void StartErasing(object sender, Oculus_ControllerArgs e)
    {
        marker.startFreeErasing();
    }
    void EndErasing(object sender, Oculus_ControllerArgs e)
    {
        if (e.axis_1d < 0.5f)
            marker.stopFreeErasing();
    }
}
