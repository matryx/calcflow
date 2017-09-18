using UnityEngine;
using System.Collections;

public class InitAttachment : MonoBehaviour {

    public GameObject attach;
    Transform pointerTip = null;

    public Transform tip
    {
        get
        {
            return pointerTip;
        }
    }

	// Use this for initialization
	void Start () {
    }
	
	// Update is called once per frame
	void Update () {
        Transform tip;
	    if(pointerTip == null && GetComponent<SteamVR_TrackedObject>().index != SteamVR_TrackedObject.EIndex.None)
        {
            tip = transform.Find("Model").Find("tip").Find("attach");
            GameObject attachment = (GameObject)Instantiate(attach, new Vector3(0, 0, -0.202f), Quaternion.identity);
            attachment.transform.SetParent(tip, false);
            pointerTip = attachment.transform.Find("pointerTip");
        }
	}
}
