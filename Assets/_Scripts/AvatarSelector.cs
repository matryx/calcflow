using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AvatarSelector : MonoBehaviour {

    public static AvatarSelector Avatar;

    //public AvatarTypes Type;
    private GameObject OculusAvatar;
    private GameObject SteamAvatar;

    //public enum AvatarTypes
    //{
    //    OPEN,
    //    OCULUS
    //}

	// Use this for initialization
	void Awake () {
        Debug.Log(UnityEngine.VR.VRDevice.model);
        Avatar = this;
        OculusAvatar = transform.Find("OculusAvatar").gameObject;
        SteamAvatar = transform.Find("SteamAvatar").gameObject;

        if (UnityEngine.VR.VRDevice.model.IndexOf("Rift") > 0)
        {
            SteamAvatar.SetActive(false);
            OculusAvatar.SetActive(true);
        }
        else
        {
            SteamAvatar.SetActive(true);
            OculusAvatar.SetActive(false);
        }
    }

}
