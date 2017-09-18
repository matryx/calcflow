using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ScrollBar : MonoBehaviour
{
    Transform bar, scroller;
    Scroll.orientation orientation;

    float scrollerHeight, scrollerWidth;
    int numPages, currPage;
    public float moveSpeed;

    void Awake()
    {
        currPage = 1; 
    }

    public void moveScroller(Scroll.direction dir)
    {
        float newY = (dir == Scroll.direction.DOWN) ? scroller.localPosition.y + scrollerHeight :
                      scroller.localPosition.y - scrollerHeight;

        float newX = (dir == Scroll.direction.RIGHT) ? scroller.localPosition.x - scrollerWidth :
                      scroller.localPosition.x + scrollerWidth;

        Vector3 newPos = (orientation == Scroll.orientation.VERTICAL) ?
                          new Vector3(scroller.localPosition.x, newY, scroller.localPosition.z) :
                          new Vector3(newX, scroller.localPosition.y, scroller.localPosition.z);

        if (orientation == Scroll.orientation.VERTICAL)
        {
            currPage = (dir == Scroll.direction.UP) ? ++currPage : --currPage;
        }
        else if (orientation == Scroll.orientation.HORIZONTAL)
        {
            currPage = (dir == Scroll.direction.LEFT) ? ++currPage : --currPage;
        }

        StartCoroutine(MoveTo(scroller.localPosition, newPos, moveSpeed));
    }

    //should only be called if numpages changed
    public void setNumPages(int num)
    {
        numPages = num;
        if (currPage > numPages) currPage = numPages;

        if (numPages == 1)
        {
            gameObject.SetActive(false);
        }
        else
        {
            gameObject.SetActive(true);
            setScroller();
        }
    }

    public int getCurrPage()
    {
        return currPage;
    }

    private void setScroller()
    {
        bar = transform.Find("Bar");
        scroller = transform.Find("Scroller");

        if (orientation == Scroll.orientation.VERTICAL)
        {
            scrollerHeight = (float)System.Math.Round((bar.localScale.y / numPages), 2);
            scroller.localScale = new Vector3(bar.localScale.x + 0.02f, scrollerHeight,
                                              bar.localScale.z + 0.02f);

            float yPos = (numPages % 2 == 0) ?
                         (float)System.Math.Round((scrollerHeight * ((float)numPages / 2f)), 3) -
                         (float)System.Math.Round((scrollerHeight / 2f), 3) :
                         ((numPages - 1) / 2) * scrollerHeight;

            scroller.localPosition = new Vector3(scroller.localPosition.x, yPos, scroller.localPosition.z);

            scroller.localPosition = new Vector3(scroller.localPosition.x,
                                                 scroller.localPosition.y - ((currPage - 1) * scrollerHeight),
                                                 scroller.localPosition.z);
        }
        else
        {
            scrollerWidth = (float)System.Math.Round((bar.localScale.x / numPages), 2);
            scroller.localScale = new Vector3(scrollerWidth, bar.localScale.y + 0.02f,
                                              bar.localScale.z + 0.02f);

            float xPos = (numPages % 2 == 0) ?
                         (float)System.Math.Round((scrollerWidth * ((float)numPages / 2f)), 3) -
                         (float)System.Math.Round((scrollerWidth / 2f), 3) :
                         ((numPages - 1) / 2) * scrollerWidth;

            scroller.localPosition = new Vector3(-xPos, scroller.localPosition.y, scroller.localPosition.z);

            scroller.localPosition = new Vector3(scroller.localPosition.x + ((currPage - 1) * scrollerWidth),
                                                 scroller.localPosition.y, scroller.localPosition.z);
        }
    }

    public void setOrientation(Scroll.orientation or)
    {
        orientation = or;
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

    void Update()
    {

    }
}
