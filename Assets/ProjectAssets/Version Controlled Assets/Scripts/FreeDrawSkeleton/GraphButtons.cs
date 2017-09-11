using UnityEngine;
using System.Collections;

public class GraphButtons : MonoBehaviour {

	// Use this for initialization
	void Start () {
	
	}
	
	// Update is called once per frame
	void Update () {
	
	}

    void OnTriggerEnter()
    {
        if (transform.name.Equals("Undo"))
        {
            transform.parent.GetComponent<Graph>().undoFreeAction();
        } else if (transform.name.Equals("Redo"))
        {
            transform.parent.GetComponent<Graph>().redoFreeAction();
        }
    }
}
