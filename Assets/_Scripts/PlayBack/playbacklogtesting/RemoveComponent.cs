using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class RemoveComponent : MonoBehaviour {

	public Component toDelete;
	
	// Update is called once per frame
	void Update () {
		gameObject.RemoveComponent<Component>(toDelete);
		Destroy(this);
	}
}
