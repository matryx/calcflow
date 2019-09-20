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
		title.text = "Enable";
		description.text = "Click to enable\nmovement of\nvectors by hand.";
	}

	public void LockOff() {
		radio.enabled = false;
		lockIcon.enabled = false;
		fixFeedback.material.color = passive;
		title.text = "Disable";
		description.text = "Click to disable\nmovement of\nvectors by hand.";
	}
}
