using System.Collections;
using System.Collections.Generic;
using UnityEngine.EventSystems;
using UnityEngine;

public class loadonplay : MonoBehaviour {

	// Use this for initialization
	void Start () {
        //UnityEngine.SceneManagement.SceneManager.LoadScene("Assets/_Scenes/8 - DoubleIntegral.unity");
        GameObject dub = GameObject.Find("DoubleIntegralScene");
        // dub.GetComponent<TouchButton>().OnButtonEnter += dub.GetComponent<TouchButton>().PressButton;
        RayCastButton rcButton = dub.GetComponent<RayCastButton>();
       // dub.GetComponent<TouchRayButton>().onClick.invoke();
       RayCastButton button = rcButton;
        if (button != null)
        {
            button.PressButton(dub);
        }
        //ExecuteEvents.Execute(dub.GetComponent<LoadSceneOnTouch>());
        // dub.GetComponent<TouchButton>().PressButton(dub);
        //dub.GetComponent<TouchButton>().
        print(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name);
        print("inside tester");
    }
	
	// Update is called once per frame
	void Update () {
		
	}
}
