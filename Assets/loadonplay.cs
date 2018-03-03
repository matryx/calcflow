using System.Collections;
using System.Collections.Generic;
using UnityEngine.EventSystems;
using UnityEngine;

public class loadonplay : MonoBehaviour {

	// Use this for initialization
	void Start () {
    }
	
	// Update is called once per frame
	void Update () {
        if(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name == "8 - DoubleIntegral.unity")
        {
            return;
        }

        GameObject dub = GameObject.Find("DoubleIntegralScene");
        // dub.GetComponent<TouchButton>().OnButtonEnter += dub.GetComponent<TouchButton>().PressButton;
        RayCastButton rcButton = dub.GetComponent<RayCastButton>();
        // dub.GetComponent<TouchRayButton>().onClick.invoke();
        RayCastButton button = rcButton;
        if (button != null)
        {
            button.PressButton(dub);
        }
        //assert this after some event?
        if (UnityEngine.SceneManagement.SceneManager.GetActiveScene().name == "8 - DoubleIntegral.unity")
        {
            print("Successfully switched scenes");
        }
    }
}
