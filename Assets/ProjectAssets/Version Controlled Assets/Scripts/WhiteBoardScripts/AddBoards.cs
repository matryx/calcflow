using UnityEngine;
using System.Collections;

public class AddBoards : MonoBehaviour {

	// Use this for initialization
	void Start () {
	
	}
	
	// Update is called once per frame
	void Update () {
	
	}

	void OnTriggerEnter() {
		transform.parent.GetComponent<BoardContainer> ().addBoards ();
	}
}
