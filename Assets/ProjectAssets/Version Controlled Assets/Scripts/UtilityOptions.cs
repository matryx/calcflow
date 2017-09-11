using UnityEngine;
using System.Collections;

public class UtilityOptions : MonoBehaviour {

    Transform vo;

	// Use this for initialization
	void Start () {
        if (transform.Find("Description"))
        {
            if (GameOptions.instance.options[0])
            {
                transform.Find("Description").gameObject.SetActive(true);
            }
            else
            {
                transform.Find("Description").gameObject.SetActive(false);
            }
        }

        if (vo = transform.Find("Voiceover"))
        {
            if (GameOptions.instance.options[2])
            {
                //transform.FindChild("Voiceover").GetComponent<AudioSource>().Play();
                vo.GetComponent<AudioSource>().Play();
            }
        }
	}
	
	// Update is called once per frame
	void Update () {

	}
}
