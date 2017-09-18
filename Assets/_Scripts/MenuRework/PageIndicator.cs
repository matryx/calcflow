using System.Collections;
using System.Collections.Generic;
using System;
using UnityEngine;

public class PageIndicator : MonoBehaviour {
    Transform backboard, scroller;
    int numPages;
    int up = 1;
    int down = 0;
    float scrollerHeight;

    // Use this for initialization
    public PageIndicator Initialize() {
        backboard = transform.Find("Backboard");
        scroller = transform.Find("Scroller");
        return this;
    }

    IEnumerator MoveTo(Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            scroller.localPosition = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        scroller.localPosition = end;
    }

    public void moveScroller(int menuDir)
    {
        float newY = (menuDir == down) ? scroller.localPosition.y + scrollerHeight:
                                         scroller.localPosition.y - scrollerHeight;
        Vector3 newPos = new Vector3(scroller.localPosition.x, newY, scroller.localPosition.z);
        StartCoroutine(MoveTo(scroller.localPosition, newPos, 0.25f));
    }

    public void setNumPages(int num)
    {
        numPages = num;
        scrollerHeight = (float)System.Math.Round((backboard.localScale.y / numPages), 2);
        scroller.localScale = new Vector3(scroller.localScale.x, scrollerHeight,
                                          scroller.localScale.z);

        gameObject.SetActive(numPages != 1);

        MoveToTop();
    }
	
    public void setCurrentPage(int num)
    {
        MoveToTop();

        scroller.localPosition = new Vector3(scroller.localPosition.x, scroller.localPosition.y + (num-1)*-scrollerHeight,
                                             scroller.localPosition.z);
    }

    private void MoveToTop()
    {
        float yPos;
        if (numPages % 2 == 0)
        {
            yPos = (float)System.Math.Round((scrollerHeight * ((float)numPages / 2f)), 3) -
                   (float)System.Math.Round((scrollerHeight / 2f), 3);
        }
        else
        {
            yPos = ((numPages - 1) / 2) * scrollerHeight;
        }

        scroller.localPosition = new Vector3(scroller.localPosition.x, yPos,
                                             scroller.localPosition.z);
    }

    //handle these when numpages updated
    public void addPage()
    {
        numPages++;
        scrollerHeight = (float)System.Math.Round((backboard.localScale.y / numPages), 2);
        scroller.localScale = new Vector3(scroller.localScale.x, scrollerHeight,
                                          scroller.localScale.z);
        MoveToTop();
    }

    public void removePage()
    {
        numPages--;
        scrollerHeight = (float)System.Math.Round((backboard.localScale.y / numPages), 2);
        scroller.localScale = new Vector3(scroller.localScale.x, scrollerHeight,
                                          scroller.localScale.z);
        MoveToTop();
    }

    // Update is called once per frame
    void Update () {
		
	}
}
