using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class testScript : MonoBehaviour {

	// Use this for initialization
	GameObject frameObj;
	LineRenderer frameLine;
	float resolution = 25f;
	public int numPoints = 10;
	public Material frameMaterial;
	public Transform point;
	List<string> currPrices = new List<string>();
	void Start () {
		frameObj = transform.gameObject;

		frameLine = frameObj.AddComponent<LineRenderer>();

		frameLine.startWidth = 0.02f;
		frameLine.endWidth = 0.02f;
		frameLine.positionCount = numPoints;
		frameLine.material = frameMaterial;


		frameLine.useWorldSpace = false;

		readPrices datas = new readPrices();
		//datas.Start();
		currPrices = null;
		Debug.Log("prices: " + currPrices.Count);

		float[] Ys = new float [numPoints];

		for (int i = 0; i < currPrices.Count; ++i) {
			//Ys [i] = Random.Range (-2f, 2f);
			float xPos = ((float)i)/ (currPrices.Count/resolution) - 12.5f;
			frameLine.SetPosition (i, new Vector3 (xPos, float.Parse(currPrices [i]), 0));
			Transform currPoint = Instantiate (point, new Vector3 (0, 0, 0), Quaternion.identity, transform);
			currPoint.localPosition = new Vector3 (xPos, float.Parse(currPrices [i]), 0);

		}

	}
	
	// Update is called once per frame
	void Update () {

	}
}
