using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ScrollBar : MonoBehaviour
{
    Transform bar, scroller, scroll;
    Scroll.orientation orientation;
    Scroll.placement placement;

    float scrollerHeight, scrollerWidth;
    int numPages, currPage = 1;
    public float moveSpeed;
    Material scrollerMaterial;

    void Awake()
    {
        scroll = transform.parent.GetComponentInChildren<Scroll>().transform;
        scrollerMaterial = scroll.GetComponent<Scroll>().scrollerMaterial;
        orientation = scroll.GetComponent<Scroll>().GetOrientation();
        placement = scroll.GetComponent<Scroll>().GetScrollBarPlacement();
        currPage = 1;

        if (bar == null || scroller == null) initializeScrollBar();
    }

    public void initializeScrollBar()
    {
        if (bar && scroller) return;

        if (scroll == null) scroll = transform.parent.GetComponentInChildren<Scroll>().transform;
        scrollerMaterial = scroll.GetComponent<Scroll>().scrollerMaterial;
        orientation = scroll.GetComponent<Scroll>().GetOrientation();
        placement = scroll.GetComponent<Scroll>().GetScrollBarPlacement();
        currPage = 1;

        bar = GameObject.CreatePrimitive(PrimitiveType.Cube).transform;
        bar.name = "Bar";
        bar.GetComponent<Renderer>().material = scroll.GetComponent<Renderer>().material;
        bar.transform.SetParent(scroll.parent);
        bar.transform.localPosition = Vector3.zero;
        bar.transform.localEulerAngles = Vector3.zero;

        float xScale = (orientation == Scroll.orientation.VERTICAL) ? 0.15f : scroll.localScale.x / 1.5f;
        float yScale = (orientation == Scroll.orientation.VERTICAL) ? scroll.localScale.y / 1.5f : 0.15f;

        bar.transform.localScale = new Vector3(xScale, yScale, scroll.localScale.z);

        float moveBy;
        if (orientation == Scroll.orientation.VERTICAL)
        {
            moveBy = (scroll.localScale.x / 2) + (bar.transform.localScale.x / 2) + 0.1f;

            bar.transform.localPosition = (placement == Scroll.placement.RIGHT) ?
                                           new Vector3(moveBy, 0, 0) : new Vector3(-moveBy, 0, 0);
        }
        else
        {
            moveBy = (scroll.localScale.y / 2) + (bar.transform.localScale.y / 2) + 0.1f;

            bar.transform.localPosition = (placement == Scroll.placement.BOTTOM) ?
                                           new Vector3(0, -moveBy, 0) : new Vector3(0, moveBy, 0);
        }

        scroller = GameObject.CreatePrimitive(PrimitiveType.Cube).transform;
        scroller.name = "Scroller";
        scroller.GetComponent<Renderer>().material = scrollerMaterial;
        scroller.transform.SetParent(transform);
        scroller.transform.localScale = Vector3.zero;
        scroller.transform.localPosition = Vector3.zero;
        scroller.transform.localEulerAngles = Vector3.zero;

        transform.SetParent(bar.transform);
        transform.localPosition = Vector3.zero;
        transform.SetParent(scroll.parent);
        bar.transform.SetParent(transform);
        gameObject.SetActive(false);
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

        if (numPages == 1 || numPages == 0)
        {
            gameObject.SetActive(false);
        }
        else
        {
            gameObject.SetActive(true);
            setUpScrollBar();
        }
    }

    public int getCurrPage()
    {
        return currPage;
    }

    private void setUpScrollBar()
    {
        float xScale = (orientation == Scroll.orientation.VERTICAL) ? 0.15f : scroll.localScale.x / 1.5f;
        float yScale = (orientation == Scroll.orientation.VERTICAL) ? scroll.localScale.y / 1.5f : 0.15f;

        bar.transform.localScale = new Vector3(xScale, yScale, scroll.localScale.z);

        if (orientation == Scroll.orientation.VERTICAL)
        {
            scrollerHeight = (float)System.Math.Round((bar.localScale.y / numPages), 2) + 0.01f;
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
