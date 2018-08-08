using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
using System.Text;
using UnityEngine.Networking;
using Nanome.Core;

public class LineChart : MonoBehaviour
{

    private static LineChart _instance;
    public static LineChart GetInstance()
    {
        return _instance;
    }

    private string URL;
    public void SetURL(string newURL)
    {
        URL = newURL;
    }

    // Use this for initialization
    string output;
    StringBuilder builder = new StringBuilder();
    public List<string> times = new List<string>();
    public List<string> prices = new List<string>();

    public List<GameObject> pointList = new List<GameObject>();
    float[] scaledPrices;

    GameObject frameObj;
    LineRenderer frameLine;

    // The factor by which number of points are determined. 1 = full points = lag
    public int numPoints = 3;

    // Higher resolution will stretch out the graph, lower will compress it.
    float resolution = 25f;
    //public int numPoints = 10;
    public Material frameMaterial;
    public Transform point;

    void Awake()
    {
        _instance = this;
    }

    void Start()
    {
        DateTime epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
        double currTime = Math.Round((double)(DateTime.UtcNow - epoch).TotalMilliseconds);
        Debug.Log("CURRTIME: " + currTime);
        SetURL("https://graphs2.coinmarketcap.com/currencies/bitcoin/0/" + currTime + "/");
        updateGraph();
    }

    public void kill()
    {
        StopAllCoroutines();
        Destroy(frameLine);
        times = new List<string>();
        prices = new List<string>();

        foreach (Transform child in transform)
        {
            Destroy(child.gameObject);
        }

        pointList = new List<GameObject>();
        builder = new StringBuilder();
    }
    public void updateGraph()
    {
        Async obj = Async.runInCoroutine(GetText);
        obj.onEvent("Done", parseData);
    }

    // Extracts the usd price time/price data from the text
    void parseData(object tmp)
    {
        Debug.Log("parseData");
        string data = ((StringBuilder)tmp).ToString();
        int startIndex = data.IndexOf("price_usd");
        int endIndex = data.IndexOf("]]", startIndex);

        //Debug.Log ("start: " + startIndex + ", End: " + endIndex);
        data = data.Substring(startIndex + 13, endIndex - startIndex);
        fillData(data);
        //Debug.Log (data);
    }

    // Separates the time and price into separate lists
    void fillData(string allData)
    {
        while (allData.IndexOf("]]") > 5)
        {
            int endIndex = allData.IndexOf(",");
            times.Add(allData.Substring(1, endIndex - 1));

            allData = allData.Substring(endIndex + 2, allData.Length - endIndex - 2);
            endIndex = allData.IndexOf("]");
            prices.Add(allData.Substring(0, endIndex));
            allData = allData.Substring(endIndex + 3, allData.Length - endIndex - 3);
        }

        Debug.Log("Times: " + times.Count + ", Prices: " + prices.Count);
        Debug.Log("lastTime: " + times[times.Count - 1] + ", lastPrice: " + prices[prices.Count - 1]);

        makeGraph(times, prices);
    }

    void makeGraph(List<string> times, List<string> prices)
    {
        this.times = times;
        this.prices = prices;

        frameObj = transform.gameObject;

        // Setting up the linerenderer
        frameLine = frameObj.AddComponent<LineRenderer>();
        frameLine.startWidth = 0.02f;
        frameLine.endWidth = 0.02f;
        frameLine.positionCount = prices.Count;
        frameLine.material = frameMaterial;
        frameLine.useWorldSpace = false;

        string[] Ys = prices.ToArray();
        rescaleData(Ys, 0);

        //Debug.Log("test: " + prices.Count);

        // Creates lines between points, places point prefabs
        for (int i = 0; i < prices.Count; ++i)
        {
            //Ys [i] = Random.Range (-2f, 2f);
            float xPos = ((float)i) / (prices.Count / resolution) - 12.5f;
            frameLine.SetPosition(i, new Vector3(xPos, scaledPrices[i], 0));
/*             if (i % numPoints == 0)
            {
                Transform currPoint = Instantiate(point, new Vector3(0, 0, 0), Quaternion.identity, transform);
                currPoint.localPosition = new Vector3(xPos, scaledPrices[i], 0);
            } */


            //pointList.Add(currPoint);


        }
    }

    // Rescales the y-values to fit between [-2,2] so that it can fit in the graph
    void rescaleData(string[] oldData, int fineTune)
    {
        scaledPrices = new float[oldData.Length];
        float max = float.Parse(oldData[0]);
        float min = float.Parse(oldData[0]);
        int minI = 0;
        for (int i = 0; i < oldData.Length; ++i)
        {

            if (float.Parse(oldData[i]) > max)
            {
                max = float.Parse(oldData[i]);
            }
            if (float.Parse(oldData[i]) < min)
            {
                min = float.Parse(oldData[i]);
                minI = i;
            }
        }

        float scale;

        Debug.Log("SCALE: " + min / max);
    scalePrices:
        scale = findScale(min, max, fineTune);
        for (int i = 0; i < oldData.Length; ++i)
        {
            scaledPrices[i] = (float.Parse(oldData[i]) / max * scale) - scale + 2;
            // Temporarily adjusts the scale if the data goes below the graph.
            if (scaledPrices[i] < -2.5)
            {
                fineTune -= 5;
                goto scalePrices;
            }
        }
    }

    // The scale, min/max, represent how close together all of the points are.
    // As scale approaces 0, the points are well disbursed. As scale approaces 1, the points 
    // Are clumped together.
    // The closer to 1 the scale is, the more the points need to be stretched out.
    float findScale(float min, float max, int fineTune)
    {
        Debug.Log("Finding Scale");
        if (min / max > 0.9f)
        {
            return min / max * (70 + fineTune);
        }
        else if (min / max > 0.8f)
        {
            return min / max * (35 + fineTune);
        }
        else if (min / max > 0.6f)
        {
            return min / max * (20 + fineTune);
        }
        else if (min / max > 0.4f)
        {
            return min / max * (10 + fineTune);
        }
        else
        {
            return 4;
        }
    }

    IEnumerator GetText(Async routine)
    {
        using (WWW www = new WWW(URL))
        {
            yield return www;
            yield return www.text;
            builder.Append(www.text);
            routine.pushEvent("Done", builder);
        }
    }

}