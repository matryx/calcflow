using UnityEngine;
using System.Collections;
using System;
using CalcFlowUI;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]
public class ToggleButtons : QuickButton {
	public GameObject field;

	// Use this for initialization


	public void ToggleButton(GameObject other) {
        if (other.gameObject.layer != LayerMask.NameToLayer("ButtonPresser")) return;
        if (SoundFXManager.instance != null)
		{
			SoundFXManager.instance.PlayClickFX();
		}

		if (field.activeSelf) {
			field.SetActive (false);
		} else {
			field.SetActive (true);
		}
	}

    protected override void ButtonEnterBehavior(GameObject other)
    {
        ToggleButton(other);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
    }
}
