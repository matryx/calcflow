using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
using System.Text;
using UnityEngine.Networking;
using Nanome.Core;

public class testreadPrices : MonoBehaviour {
/* 
	// Use this for initialization
	string output;
	StringBuilder builder = new StringBuilder();
	List<string> times = new List<string>();
	List<string> prices = new List<string>();

	void begin () {
		//builder.Append ("test");
		Async obj = Async.runInCoroutine(GetText);
		obj.onEvent ("Done", parseData);
	}

	public List<string> getTimes(){
		return times;
	}
	public List<string> getPrices(){
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

		//Debug.Log ("Times: " + times.Count + ", Prices: " + prices.Count);
		//Debug.Log ("lastTime: " + times[times.Count-1] + ", lastPrice: " + prices[prices.Count-1]);
	}

	IEnumerator GetText(Async routine){
		string url = "https://graphs2.coinmarketcap.com/currencies/matryx/1531333449000/1531419849000/";
		using(WWW www = new WWW (url)) {
			yield return www;
			yield return www.text;
			builder.Append (www.text);
			routine.pushEvent("Done", builder);
		}
	}
		*/
}
