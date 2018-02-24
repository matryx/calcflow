using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class loadonplay : MonoBehaviour {

	// Use this for initialization
	void Start () {
        //UnityEngine.SceneManagement.SceneManager.LoadScene("Assets/_Scenes/8 - DoubleIntegral.unity");
        GameObject dub = GameObject.Find("DoubleIntegralScene");
        dub.GetComponent<TouchButton>().OnButtonEnter += dub.GetComponent<TouchButton>().PressButton;
        dub.GetComponent<TouchButton>().PressButton(dub);
        print(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name);
    }
	
	// Update is called once per frame
	void Update () {
		
	}
}
