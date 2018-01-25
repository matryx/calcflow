using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

public class ClearObjects : MonoBehaviour {

	public bool clearAll = false;

	// Use this for initialization
	void Start () {
		
	}
	
	void Update(){
		if (clearAll) {
			ClearAllObjects();
			clearAll = false;
		}
	}
	// Update is called once per frame
	void ClearAllObjects () {
		UIDSystem[] objects = FindObjectsOfType<UIDSystem>();
		foreach (UIDSystem u in objects){
			if(u.gameObject != gameObject)
				Destroy(u.gameObject);
		}
	}

}
