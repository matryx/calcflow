using UnityEngine;
using System.Collections;

public class Oculus_GrabbingInteractions : MonoBehaviour
{

    //Oculus_TrackedController controller;
    //GrabbableObject obj;
    //float grabThreshold;
    //Oculus_HandAnim anim;
    //bool isSelect = false;

    //void Awake()
    //{
    //    grabThreshold = 0.5f;
    //    obj = null;
    //    controller = GetComponent<Oculus_TrackedController>();
    //    anim = GetComponent<Oculus_HandAnim>();
    //}

    //void Start()
    //{
    //    //Debug.Log("initialPos = " + GameOptions.initialCameraPos);
    //    //if (GameOptions.initialCameraPos != Vector3.zero)
    //    //{
    //    //    Transform cameraTransform = transform.parent.parent.parent.parent;
    //    //    if (cameraTransform != null)
    //    //    {
    //    //        cameraTransform.position = GameOptions.initialCameraPos;
    //    //        cameraTransform.rotation = GameOptions.initialCameraRot;
    //    //    }
    //    //}
    //}

    //void OnEnable()
    //{
    //    anim.SetFlex(0f);
    //    anim.SetPoint(false);
    //    anim.SetThumb(false);
    //    anim.SetPose(0);
    //    controller.handTriggerPressing += GrabAnim;
    //    controller.handTriggerUnpressing += ReleaseAnim;
    //    controller.indexTriggerUntouched += EnablePoke;
    //    controller.indexTriggerTouched += DisablePoke;
    //    controller.thumbTouched += HoldThumb;
    //    controller.thumbUntouched += ReleaseThumb;
    //    controller.menubuttonPressed += Recenter;
    //}

    //void OnDisable()
    //{
    //    controller.handTriggerPressing -= GrabAnim;
    //    controller.handTriggerUnpressing -= ReleaseAnim;
    //    controller.indexTriggerUntouched -= EnablePoke;
    //    controller.indexTriggerTouched -= DisablePoke;
    //    controller.thumbTouched -= HoldThumb;
    //    controller.thumbUntouched -= ReleaseThumb;
    //    controller.menubuttonPressed -= Recenter;
    //}



    //void GrabAnim(object sender, Oculus_ControllerArgs e)
    //{
    //    if (obj != null || e.axis_1d < grabThreshold || isSelect)
    //    //if(obj != null || e.axis_1d < grabThreshold )
    //    {
    //        if (anim)
    //        {
    //            anim.SetFlex(e.axis_1d);
    //        }
    //        return;
    //    }

    //    if (anim)
    //    {
    //        anim.SetFlex(e.axis_1d);
    //        //anim.SetPose(closest ? 1 : 0);
    //    }
    //}

    //void ReleaseAnim(object sender, Oculus_ControllerArgs e)
    //{
    //    if (anim)
    //    {
    //        anim.SetPose(0);
    //        anim.SetFlex(e.axis_1d);
    //    }
    //}

    //void HoldThumb(object sender, Oculus_ControllerArgs e)
    //{
    //    if (anim) anim.SetThumb(false);
    //}

    //void ReleaseThumb(object sender, Oculus_ControllerArgs e)
    //{
    //    if (anim) anim.SetThumb(true);
    //}

    //void EnablePoke(object sender, Oculus_ControllerArgs e)
    //{
    //    if (controller.handedness == OvrTouch.Controllers.HandednessId.Left)
    //        transform.Find("l_hand_skeletal").Find("hands:l_hand_world").Find("hands:b_l_hand").Find("hands:b_l_index1").Find("hands:b_l_index2").Find("hands:b_l_index3").Find("coll_hands:b_l_index3").GetComponent<CapsuleCollider>().enabled = true;
    //    else
    //        transform.Find("r_hand_skeletal").Find("hands:r_hand_world").Find("hands:b_r_hand").Find("hands:b_r_index1").Find("hands:b_r_index2").Find("hands:b_r_index3").Find("coll_hands:b_r_index3").GetComponent<CapsuleCollider>().enabled = true;
    //    if (anim) anim.SetPoint(true);
    //    isSelect = true;
    //}

    //void DisablePoke(object sender, Oculus_ControllerArgs e)
    //{
    //    if (controller.handedness == OvrTouch.Controllers.HandednessId.Left)
    //        transform.Find("l_hand_skeletal").Find("hands:l_hand_world").Find("hands:b_l_hand").Find("hands:b_l_index1").Find("hands:b_l_index2").Find("hands:b_l_index3").Find("coll_hands:b_l_index3").GetComponent<CapsuleCollider>().enabled = false;
    //    else
    //        transform.Find("r_hand_skeletal").Find("hands:r_hand_world").Find("hands:b_r_hand").Find("hands:b_r_index1").Find("hands:b_r_index2").Find("hands:b_r_index3").Find("coll_hands:b_r_index3").GetComponent<CapsuleCollider>().enabled = false;
    //    if (anim) anim.SetPoint(false);
    //    isSelect = false;
    //}

    //void Recenter(object sender, Oculus_ControllerArgs e)
    //{
    //    Transform oculus_transform = transform.parent.parent.parent.parent;
    //    if (oculus_transform.gameObject.name == "Oculus")
    //    {
    //        Quaternion eyeRot = transform.parent.parent.Find("CenterEyeAnchor").rotation;
    //        eyeRot.x = 0f;
    //        eyeRot.z = 0f;
    //        oculus_transform.rotation *= Quaternion.Inverse(eyeRot);

    //        Vector3 eyePos = transform.parent.parent.Find("CenterEyeAnchor").position;
    //        oculus_transform.position -= eyePos;
    //        oculus_transform.position = new Vector3(oculus_transform.position.x, 10f, oculus_transform.position.z);

    //        GameOptions.initialCameraPos = oculus_transform.position;
    //        GameOptions.initialCameraRot = oculus_transform.rotation;
    //    }
    //}
}
