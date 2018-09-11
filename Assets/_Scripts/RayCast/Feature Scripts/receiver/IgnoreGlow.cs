using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class IgnoreGlow : MonoBehaviour {

    int layerIndex;
    // Use this for initialization
    void Start()
    {
        layerIndex = gameObject.layer;
    }
	
	// Update is called once per frame
	void LateUpdate () {
        if (gameObject.layer == LayerMask.NameToLayer("Glow"))
        {
            gameObject.layer = layerIndex;
        }
	}
}
