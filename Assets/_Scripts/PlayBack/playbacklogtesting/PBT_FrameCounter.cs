using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PBT_FrameCounter : MonoBehaviour {

	static long frame = 0;
	void Update(){
		frame++;
	}

	public static long GetFrame(){
		return frame;
	}
}
