using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false, false)]
public class LookAtText : MonoBehaviour {

    public GameObject cam;

	// Use this for initialization
	void Start () {
        if (FindObjectOfType<SteamVR_Camera>())
        {
            cam = FindObjectOfType<SteamVR_Camera>().gameObject;
        }
        else
        {
            cam = Camera.main.gameObject;
        }
	}
	
	// Update is called once per frame
	void Update () {
        transform.rotation = Quaternion.LookRotation(cam.transform.position - transform.position) * Quaternion.Euler(0, 180, 0);
	}
}
