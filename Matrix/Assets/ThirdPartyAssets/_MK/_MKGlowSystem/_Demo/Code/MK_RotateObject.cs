using UnityEngine;
using System.Collections;

public class MK_RotateObject : MonoBehaviour 
{
	public float speed;

	void Update () 
	{
		this.transform.Rotate(new Vector3(0,speed,0));
	}
}
