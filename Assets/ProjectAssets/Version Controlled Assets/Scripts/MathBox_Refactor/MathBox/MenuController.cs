using UnityEngine;
using System.Collections;
using UnityEngine.SceneManagement;

public class MenuController : MonoBehaviour {

    private SteamVR_ControllerManager man;
    private GameObject pointer;
    public MenuScript menu;

	// Use this for initialization
	void Start () {
        //Application.LoadLevel("Hackathon");
        man = GameObject.Find("[CameraRig]").GetComponent<SteamVR_ControllerManager>();
        pointer = GameObject.Find("Pointer");
	}
	
	// Update is called once per frame
	void Update () {
        //var rc = man.right.GetComponent<SteamVR_TrackedObject>();
        var lc = man.left.GetComponent<SteamVR_TrackedObject>();
        var l = SteamVR_Controller.Input((int)lc.index);

        RaycastHit hit;
        Physics.Raycast(lc.transform.position, lc.transform.forward, out hit, 1000);
        if (hit.collider) 
        {
            if (l.GetHairTriggerDown())
            {
                SceneManager.LoadScene(hit.collider.name);
            }
            pointer.transform.position = hit.point;
            pointer.SetActive(true);
        }
        else
        {
            pointer.SetActive(false);
        }
    }
}
