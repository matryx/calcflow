using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AvatarSelector : MonoBehaviour
{
    public static AvatarSelector Instance { get; private set; }
    public static GameObject Avatar;
    public static Transform centerEye;

    //public AvatarTypes Type;
    private GameObject OculusAvatar;
    private GameObject SteamAvatar;

    //public enum AvatarTypes
    //{
    //    OPEN,
    //    OCULUS
    //}

    // Use this for initialization
    void Awake()
    {
        if(Instance == null)
        {
            Instance = this;
        }

        Debug.Log(UnityEngine.XR.XRDevice.model);
        OculusAvatar = transform.Find("OculusAvatar").gameObject;
        SteamAvatar = transform.Find("SteamAvatar").gameObject;

            SteamAvatar.SetActive(false);
            OculusAvatar.SetActive(true);
            Avatar = OculusAvatar;
            centerEye = Avatar.transform.Find("OVRCameraRig/TrackingSpace/CenterEyeAnchor");


    }
}