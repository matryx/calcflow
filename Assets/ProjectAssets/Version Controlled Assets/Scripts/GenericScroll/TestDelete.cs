using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TestDelete : QuickButton {
    ActualDelete del;

	// Use this for initialization
	protected override void Start () {
        base.Start();

        del = GameObject.Find("DeleteButton").GetComponent<ActualDelete>();
	}
	
	// Update is called once per frame
	void Update () {
		
	}

    protected override void ButtonEnterBehavior(GameObject other)
    {
        del.addToDelete(transform.parent);
        print("ADDED");
    }

    protected override void ButtonExitBehavior(GameObject other)
    {

    }
}
