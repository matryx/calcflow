using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FlexButtonLockColumns : MonoBehaviour {

	public MeshRenderer radio;
	public SpriteRenderer lockIcon;
	public Renderer fixFeedback;
	public TextMesh title;
	public TextMesh description;

	public Color passive;
	Color active = new Color(0, 204, 54);

	public void LockOn() {
		radio.enabled = true;
        lockIcon.enabled = true;
        fixFeedback.material.color = active;
		title.text = "Enable Grabbable Vectors";
		description.text = "Click to unlock\nthe plane when\ndragging points.";
	}

	public void LockOff() {
		radio.enabled = false;
		lockIcon.enabled = false;
		fixFeedback.material.color = passive;
		title.text = "Disable Grabbable Vectors";
		description.text = "Click to lock\nthe plane when\ndragging points.";
	}
}
