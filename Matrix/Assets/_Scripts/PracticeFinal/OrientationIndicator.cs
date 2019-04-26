using UnityEngine;
using System.Collections;

public class OrientationIndicator : MonoBehaviour {
    float theta = 0;
    float radius;
	// Use this for initialization
	void Start () {
        radius = transform.localPosition.y;
	}
	
	// Update is called once per frame
	void Update () {
        theta = theta + Mathf.Deg2Rad ;
        float x = radius * Mathf.Cos(theta);
        float y = radius * Mathf.Sin(theta);

        transform.localPosition = new Vector3(x, y, 0);
	}
}
