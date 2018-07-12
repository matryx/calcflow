using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
using System.Text;
using UnityEngine.Networking;
using Nanome.Core;

public class readPrices : MonoBehaviour {

	// Use this for initialization
	string output;
	StringBuilder builder = new StringBuilder();
	public List<string> times = new List<string>();
	public List<string> prices = new List<string>();
	float[]scaledPrices;

	GameObject frameObj;
	LineRenderer frameLine;
	float resolution = 25f;
	public int numPoints = 10;
	public Material frameMaterial;
	public Transform point;

	 void Start () {
		//builder.Append ("test");
		Async obj = Async.runInCoroutine(GetText);
		obj.onEvent ("Done", parseData);
	}

	public List<string> getTimes(){
		return times;
	}
	public List<string> getPrices(){
		Debug.Log("OGprices:" + prices.Count);
		return prices;
	}

	void parseData(object tmp){ 
		string data = ((StringBuilder)tmp).ToString ();
		int startIndex = data.IndexOf("price_usd");
		int endIndex = data.IndexOf ("]]", startIndex);

		//Debug.Log ("start: " + startIndex + ", End: " + endIndex);
		data = data.Substring (startIndex+13, endIndex-startIndex);
		fillData (data);
		//Debug.Log (data);
	}

	void fillData(string allData){
		while(allData.IndexOf("]]") > 5){
			int endIndex = allData.IndexOf (",");
			times.Add (allData.Substring (1, endIndex - 1));

			allData = allData.Substring (endIndex + 2, allData.Length - endIndex - 2);
			endIndex = allData.IndexOf ("]");
			prices.Add (allData.Substring (0, endIndex));
			allData = allData.Substring (endIndex + 3, allData.Length - endIndex - 3);
		}

		Debug.Log ("Times: " + times.Count + ", Prices: " + prices.Count);
		Debug.Log ("lastTime: " + times[times.Count-1] + ", lastPrice: " + prices[prices.Count-1]);
	
		makeGraph(times, prices);
	}

	void makeGraph(List<string> times, List<string> prices){
		this.times = times;
		this.prices = prices;

		frameObj = transform.gameObject;

		frameLine = frameObj.AddComponent<LineRenderer>();

		frameLine.startWidth = 0.02f;
		frameLine.endWidth = 0.02f;
		frameLine.positionCount = prices.Count;
		frameLine.material = frameMaterial;
		frameLine.useWorldSpace = false;

		string[] Ys = prices.ToArray();
		rescaleData(Ys);

		Debug.Log("test: " + prices.Count);
		for (int i = 0; i < prices.Count; ++i) {
			//Ys [i] = Random.Range (-2f, 2f);
			float xPos = ((float)i)/ (prices.Count/resolution) - 12.5f;
			frameLine.SetPosition (i, new Vector3 (xPos, scaledPrices[i], 0));
			Transform currPoint = Instantiate (point, new Vector3 (0, 0, 0), Quaternion.identity, transform);
			currPoint.localPosition = new Vector3 (xPos, scaledPrices[i], 0);

		}
	}

	void rescaleData(string[] oldData){
		scaledPrices = new float[oldData.Length];
		float max = float.Parse(oldData[0]);
		for(int i = 0; i < oldData.Length; ++i){
			if(float.Parse(oldData[i]) > max){
				max = float.Parse(oldData[i]);
			}
		}

		for(int i = 0; i < oldData.Length; ++i){
			scaledPrices[i] = (float.Parse(oldData[i]) / max * 4) - 2;
		}

	}

	IEnumerator GetText(Async routine){
		string url = "https://graphs2.coinmarketcap.com/currencies/bitcoin/1499897640000/1531433640000/";
		using(WWW www = new WWW (url)) {
			yield return www;
			yield return www.text;
			builder.Append (www.text);
			routine.pushEvent("Done", builder);
		}
	}
		
}