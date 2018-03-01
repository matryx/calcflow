using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false, false)]
[RequireComponent(typeof(HiResScreenShots))]

public class SaveIconGenerator : MonoBehaviour {
    HiResScreenShots screenshotter;

	// Use this for initialization
	void Awake() {
        screenshotter = GetComponent<HiResScreenShots>();
    }

    public void TakeScreenshot(string fileName)
    {
        screenshotter.TakeHiResShot(fileName);
    }


}
