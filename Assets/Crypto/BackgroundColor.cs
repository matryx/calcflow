using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class BackgroundColor : MonoBehaviour {

	// Use this for initialization
	public SpriteRenderer sprite;
	void Start () {
		sprite = gameObject.GetComponent<SpriteRenderer>();
		sprite.color = Color.gray;
	}

}
