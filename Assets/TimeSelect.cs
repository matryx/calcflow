using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TimeSelect : MonoBehaviour
{

    public ConstraintGrabbable leftSphere, rightSphere;

    ScatterChart scatterChart;

    static TimeSelect _instance;

    public static TimeSelect GetInstance()
    {
        return _instance;
    }


    string coin = "bitcoin";

    public void setCoin(string newURL)
    {
        coin = newURL;
    }

    float leftPos;
    float rightPos;

    float oldLeft, oldRight;

    string leftTimeStamp;
    string rightTimeStamp;

    List<string> times = new List<string>();


    string ogStart, ogEnd;

    void Start()
    {
        _instance = this;
        scatterChart = ScatterChart.GetInstance();
        times = scatterChart.getTimeList();
        ogStart = scatterChart.getFirstTime();
        ogEnd = scatterChart.getFirstTime();
        leftPos = leftSphere.lastLocalPos.z + 50;
        rightPos = rightSphere.lastLocalPos.z + 50;
        leftTimeStamp = getTimestamp(leftPos / 100);
        rightTimeStamp = getTimestamp(rightPos / 100);
        oldLeft = leftPos;
        oldRight = rightPos;
    }

    void updateInfo()
    {

        Debug.Log("LEFT: " + leftPos + ", RIGHT: " + rightPos);

        leftPos = leftSphere.lastLocalPos.z + 50;
        rightPos = rightSphere.lastLocalPos.z + 50;

        oldLeft = leftPos;
        oldRight = rightPos;

        if (leftPos < rightPos)
        {
            Debug.Log("LEFT ON LEFT");
            leftTimeStamp = getTimestamp(leftPos / 100);
            rightTimeStamp = getTimestamp(rightPos / 100);
        }
        else
        {
            Debug.Log("LEFT ON RIGHT");
            leftTimeStamp = getTimestamp(rightPos / 100);
            rightTimeStamp = getTimestamp(leftPos / 100);
        }

        scatterChart.kill();
        scatterChart.SetURL("https://graphs2.coinmarketcap.com/currencies/" + coin + "/" + leftTimeStamp + "/" + rightTimeStamp + "/");
        scatterChart.updateGraph();

        StopAllCoroutines();

    }

    public void updateTimes()
    {
        times = scatterChart.getTimeList();
		leftSphere.lastLocalPos.z = -50;
		rightSphere.lastLocalPos.z = 50;
    }
    void FixedUpdate()
    {
        leftPos = leftSphere.lastLocalPos.z + 50;
        rightPos = rightSphere.lastLocalPos.z + 50;

        if (oldLeft != leftPos || oldRight != rightPos)
        {
            StartCoroutine(waiting());

        }
    }

    private IEnumerator waiting()
    {
       // Debug.Log("MOVED");
    
        while(leftSphere.IsGrabbed || rightSphere.IsGrabbed){
            yield return new WaitForSeconds(.1f);
        }

        oldLeft = leftSphere.lastLocalPos.z + 50;
        oldRight = rightSphere.lastLocalPos.z + 50;
        updateInfo();
    }

    public string getTimestamp(float percent)
    {

        Debug.Log("PERCENT: " + percent);
        Debug.Log("INDEX: " + (int)Mathf.Round((float)this.times.Count * percent));
        int index = (int)Mathf.Round((float)this.times.Count * percent);
        if (index == times.Count) index--;
        return this.times[index];

    }

}
