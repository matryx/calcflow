using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using NanoVRController;

[System.Serializable]
[RequireComponent(typeof(JoyStickReceiver))]
public class Scroll : MonoBehaviour
{
    //REQUIRED: some type of transparent shaders(not standard), Unlit/UnlitAlphaWithFade recommended

    private List<Transform> objects;
    private List<Transform> toAdd = new List<Transform>();

    private Transform scrollBar;
    private Vector3 toPos;
    private JoyStickReceiver jsReceiver;

    public Transform objectParent;
    [SerializeField]
    public Material scrollerMaterial;
    [SerializeField]
    private Vector3 margin;
    [SerializeField]
    private Vector3 padding;

    [SerializeField]
    private int fixedRowOrCol;
    [SerializeField]
    private int numberOfVisibleThings;

    [SerializeField]
    private float movementSpeed = 0.3f;
    [SerializeField]
    private float fadeSpeed = 0.15f;
    [SerializeField]
    private float fadeInDelay = 0.10f;

    [System.Serializable]
    public enum orientation { VERTICAL, HORIZONTAL }
    public enum direction { UP, DOWN, LEFT, RIGHT }

    public orientation currOrientation = orientation.VERTICAL;
    public direction currDirection = direction.UP;

    int numPages;
    int lowestVisIndex = 0;
    int highestVisIndex;

    const float transparent = 0;
    const float opaque = 1;

    bool moving = false;
    bool fading = false;
    bool adding = false;

    private void Awake()
    {
        setUpMenu();
    }

    void Start()
    {
        if (transform.localEulerAngles != Vector3.zero)
        {
            Debug.LogError("Local rotation of object with Scroll script needs to be zero");
            #if UNITY_EDITOR
            UnityEditor.EditorApplication.isPlaying = false;
            #endif
        }

        if (objectParent.localScale != Vector3.one || objectParent.localEulerAngles != Vector3.zero)
        {
            Debug.LogError("Local scale and local rotation of Object Parent needs to be one and zero");
            #if UNITY_EDITOR
            UnityEditor.EditorApplication.isPlaying = false;
            #endif
        }

        jsReceiver = GetComponent<JoyStickReceiver>();
        if (jsReceiver != null) jsReceiver.JoyStickTouched += scroll;
    }

    void scroll(VRController c, ControllerComponentArgs e)
    {
        float roundBy = 90;
        float temp = Mathf.Atan2(e.y, e.x);             //calculate angle in rad
        temp = temp * (180 / Mathf.PI);                 //rad to degree
        temp = Mathf.Round(temp / roundBy) * roundBy;   //round to nearest set degree 
        temp = (temp + 360) % 360;                      //positive
        int angle = (int)temp;

        if (e.x == 0 && e.y == 0) return;

        switch (angle)
        {
            case 0:
                if (currOrientation != orientation.HORIZONTAL) return;
                currDirection = direction.LEFT;
                break;
            case 90:
                if (currOrientation != orientation.VERTICAL) return;
                currDirection = direction.DOWN;
                break;
            case 180:
                if (currOrientation != orientation.HORIZONTAL) return;
                currDirection = direction.RIGHT;
                break;
            case 270:
                if (currOrientation != orientation.VERTICAL) return;
                currDirection = direction.UP;
                break;
        }

        moveObjects();
    }

    public void setUpMenu()
    {
        if (transform.parent.GetComponentInChildren<ScrollBar>()) return;

        if (scrollBar == null)
        {
            scrollBar = new GameObject().transform;
            scrollBar.name = "ScrollBar";
            scrollBar.SetParent(transform.parent);
            scrollBar.localScale = Vector3.one;
            scrollBar.localPosition = Vector3.zero;
            scrollBar.localEulerAngles = Vector3.zero;
            scrollBar.gameObject.AddComponent<ScrollBar>();
            scrollBar.GetComponent<ScrollBar>().setOrientation(currOrientation);
            scrollBar.GetComponent<ScrollBar>().moveSpeed = movementSpeed;
            scrollBar.GetComponent<ScrollBar>().initializeScrollBar();
        }

        Vector3 startPos = transform.localPosition;

        float xPos = startPos.x - (transform.localScale.x / 2) + margin.x;
        float yPos = startPos.y + (transform.localScale.y / 2) - margin.y;
        float zPos = startPos.z - (transform.localScale.z / 2) - margin.z;

        if (fixedRowOrCol == 1)
        {
            xPos = (currOrientation == orientation.VERTICAL) ? startPos.x : xPos;
            yPos = (currOrientation == orientation.VERTICAL) ? yPos : startPos.y;
        }

        startPos = new Vector3(xPos, yPos, zPos);
        objectParent.localPosition = startPos;
    }

    private void setNumPagesAndHighestVisIndex()
    {
        numPages = (objects.Count <= numberOfVisibleThings) ? 1 :
            1 + (int)System.Math.Ceiling((double)(objects.Count - numberOfVisibleThings) / fixedRowOrCol);
        scrollBar.GetComponent<ScrollBar>().setNumPages(numPages);

        if (scrollBar.GetComponent<ScrollBar>().getCurrPage() == numPages)
            highestVisIndex = objects.Count - 1;

        if (scrollBar.GetComponent<ScrollBar>().getCurrPage() == 1 && numPages > 1)
            highestVisIndex = numberOfVisibleThings - 1;
    }

    public void initializeObjects(List<Transform> objectList)
    {
        objects = objectList;

        setNumPagesAndHighestVisIndex();

        for (int ind = 0; ind < objects.Count; ind++)
        {
            placeObject(objects[ind], ind, false);
        }
    }

    public void addObject(Transform newObj)
    {
        adding = true;
        toAdd.Add(newObj);
    }

    private void executeAdd()
    {
        if (objects == null) objects = new List<Transform>();
        int firstNewIndex = (objects.Count > 0) ? objects.Count : 0;

        foreach (Transform t in toAdd)
        {
            objects.Add(t);
        }

        setNumPagesAndHighestVisIndex();

        foreach (Transform t in toAdd)
        {
            placeObject(t, firstNewIndex, false);
            ++firstNewIndex;
        }

        toAdd.Clear();
        adding = false;
    }

    private void placeObject(Transform obj, int ind, bool deleting)
    {
        obj.SetParent(objectParent);
        obj.transform.localEulerAngles = Vector3.zero;

        float offset = (float)System.Math.Floor((double)ind / fixedRowOrCol);

        float xPos = (currOrientation == orientation.VERTICAL) ?
                      padding.x * (ind % fixedRowOrCol) : padding.x * offset;
        float yPos = (currOrientation == orientation.VERTICAL) ?
                      -padding.y * offset : -padding.y * (ind % fixedRowOrCol);

        float xOffset = (highestVisIndex == objects.Count - 1 && deleting) ?
                         xPos - (padding.x * (numPages - 1)) : xPos + objects[0].localPosition.x;
        float yOffset = (highestVisIndex == objects.Count - 1 && deleting) ?
                         yPos + (padding.y * (numPages - 1)) : yPos + objects[0].localPosition.y;

        Vector3 offsetToFirst = (currOrientation == orientation.VERTICAL) ?
                                 new Vector3(xPos, yOffset, objectParent.localPosition.z) :
                                 new Vector3(xOffset, yPos, objectParent.localPosition.z);

        Vector3 newPos = (lowestVisIndex == 0) ?
                          new Vector3(xPos, yPos, objectParent.localPosition.z) : offsetToFirst;

        if (deleting)
        {
            StartCoroutine(MoveTo(obj, obj.localPosition, newPos, movementSpeed));
        }
        else
        {
            obj.localPosition = newPos;
        }

        if (ind < lowestVisIndex || ind > highestVisIndex)
        {
            obj.gameObject.SetActive(false);
        }
        else
        {
            if (deleting && !obj.gameObject.activeSelf) StartCoroutine(FadeButtonIn(obj));
            obj.gameObject.SetActive(true);
        }
    }

    public void deleteObjects(List<Transform> objs)
    {
        List<int> indeces = new List<int>();

        for (int i = 0; i < objects.Count; i++)
        {
            Transform o = objects[i];
            foreach (Transform obj in objs)
            {
                if (o == obj)
                {
                    indeces.Add(i);
                    break;
                }
            }
        }

        indeces.Reverse(); //delete from end of list to beginning so that indeces don't get messed up

        for (int i = 0; i < indeces.Count; i++)
        {
            Transform d = objects[indeces[i]];
            objects.Remove(d);
            Destroy(d.gameObject);
        }

        int prevNum = numPages;
        numPages = (objects.Count <= numberOfVisibleThings) ? 1 :
                    1 + (int)System.Math.Ceiling((double)(objects.Count - numberOfVisibleThings) / fixedRowOrCol);
        scrollBar.GetComponent<ScrollBar>().setNumPages(numPages);

        if (highestVisIndex > objects.Count - 1) //if now on the last page
        {
            highestVisIndex = objects.Count - 1;

            if (highestVisIndex - lowestVisIndex + 1 <= numberOfVisibleThings - fixedRowOrCol)
            {
                int offsetBy = (numberOfVisibleThings / fixedRowOrCol) -
                               (int)Mathf.Ceil(((float)(highestVisIndex - lowestVisIndex + 1) / (float)fixedRowOrCol));
                lowestVisIndex = lowestVisIndex - (fixedRowOrCol * offsetBy);
            }

            if (numPages == 1) lowestVisIndex = 0;
        }

        for (int i = 0; i < objects.Count; i++) placeObject(objects[i], i, true);
    }

    private void moveObjects()
    {
        if (objects == null || objects.Count == 0) return;

        if (((currDirection == direction.DOWN || currDirection == direction.RIGHT) && lowestVisIndex == 0) ||
            ((currDirection == direction.UP || currDirection == direction.LEFT) && highestVisIndex == objects.Count - 1))
            return;

        if (!moving && !fading)
        {
            moving = true;
            fading = true;

            for (int i = 0; i < objects.Count; i++)
            {
                Transform obj = objects[i];

                float newX = (currDirection == direction.RIGHT) ?
                             (float)System.Math.Round((double)obj.localPosition.x + padding.x, 2) :
                             (float)System.Math.Round((double)obj.localPosition.x - padding.x, 2);

                float newY = (currDirection == direction.UP) ?
                             (float)System.Math.Round((double)obj.localPosition.y + padding.y, 2) :
                             (float)System.Math.Round((double)obj.localPosition.y - padding.y, 2);

                Vector3 newPos = (currOrientation == orientation.VERTICAL) ?
                                  new Vector3(obj.localPosition.x, newY, obj.localPosition.z) :
                                  new Vector3(newX, obj.localPosition.y, obj.localPosition.z);

                if (i == 0) toPos = newPos;

                StartCoroutine(MoveTo(obj, obj.localPosition, newPos, movementSpeed));

                if (currDirection == direction.UP || currDirection == direction.LEFT)
                {
                    if (i >= lowestVisIndex && i < lowestVisIndex + fixedRowOrCol)
                    {
                        FadeButtonOut(obj);
                    }
                    else if (i > highestVisIndex && i <= highestVisIndex + fixedRowOrCol)
                    {
                        StartCoroutine(FadeButtonIn(obj));
                    }
                }
                else if (currDirection == direction.DOWN || currDirection == direction.RIGHT)
                {
                    if (i < lowestVisIndex && i >= lowestVisIndex - fixedRowOrCol)
                    {
                        StartCoroutine(FadeButtonIn(obj));
                    }
                    else if (i <= highestVisIndex && i > highestVisIndex - fixedRowOrCol)
                    {
                        if (((highestVisIndex + 1) % fixedRowOrCol == 0) ||
                            (i > (highestVisIndex - ((highestVisIndex + 1) % fixedRowOrCol))))
                            FadeButtonOut(obj);
                    }
                }
            }

            scrollBar.GetComponent<ScrollBar>().moveScroller(currDirection);
            setLowAndHighIndeces();
        }
    }

    void setLowAndHighIndeces()
    {
        if (currDirection == direction.UP || currDirection == direction.LEFT)
        {
            lowestVisIndex += fixedRowOrCol;
            highestVisIndex += fixedRowOrCol;

            if (highestVisIndex > objects.Count - 1) highestVisIndex = objects.Count - 1;
        }
        else
        {
            lowestVisIndex -= fixedRowOrCol;
            highestVisIndex -= fixedRowOrCol;

            if (highestVisIndex - lowestVisIndex != numberOfVisibleThings - 1)
                highestVisIndex = lowestVisIndex + numberOfVisibleThings - 1;
        }
    }

    void FadeButtonOut(Transform obj)
    {
        AnimateFade(obj, obj.GetComponentInChildren<Renderer>().material.GetColor("_Color").a, transparent, fadeSpeed);
    }

    void AnimateFade(Transform obj, float from, float to, float time)
    {
        foreach (Transform child in obj)
        {
            StartCoroutine(FadeObj(child, from, to, time));
        }
    }

    #region Coroutines
    IEnumerator FadeButtonIn(Transform obj)
    {
        yield return new WaitForSeconds(fadeInDelay);

        obj.gameObject.SetActive(true);
        AnimateFade(obj, obj.GetComponentInChildren<Renderer>().material.GetColor("_Color").a, opaque, fadeSpeed);
    }

    IEnumerator FadeObj(Transform obj, float start, float end, float time)
    {
        Material mat = obj.GetComponent<Renderer>().material;
        string colorName = "_Color";
        Color col = Color.white;

        if (mat.HasProperty("_FaceColor")) colorName = "_FaceColor";
        if (mat.HasProperty(colorName)) col = mat.GetColor(colorName);

        for (float i = 0.0f; i < 1.0f; i += Time.deltaTime * (1 / time))
        {
            col = mat.GetColor(colorName);
            col.a = Mathf.Lerp(start, end, i);
            obj.GetComponent<Renderer>().material.SetColor(colorName, col);
            yield return null;
        }

        col.a = end;
        obj.GetComponent<Renderer>().material.SetColor(colorName, col);
        fading = false;

        if (end == transparent) obj.parent.gameObject.SetActive(false);
    }

    IEnumerator MoveTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            if (obj == null) break;
            obj.localPosition = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        if (obj != null) obj.localPosition = end;
    }
    #endregion

    void Update()
    {
        if (moving && objects.Count > 0 && objects[0].localPosition == toPos) moving = false;

        if (adding && !moving) executeAdd();
    }
}
