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

    public enum orientation { VERTICAL, HORIZONTAL }
    public enum placement { RIGHT, BOTTOM, LEFT, TOP }
    public enum direction { UP, DOWN, LEFT, RIGHT }

    [SerializeField]
    private orientation currOrientation = orientation.VERTICAL;
    [SerializeField]
    private placement scrollBarPlacement = placement.RIGHT;
    public direction currDirection = direction.UP;

    int numPages;
    int lowestVisIndex = 0;
    int highestVisIndex;
    int atIndexAdd;

    const float transparent = 0;
    const float opaque = 1;

    bool moving = false;
    bool fading = false;
    bool adding = false;
    bool setup = false;

    private void Awake()
    {
        if (setup) return;
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

    public orientation getOrientation()
    {
        return currOrientation;
    }

    public placement getScrollBarPlacement()
    {
        return scrollBarPlacement;
    }

    public void setUpMenu()
    {
        if (setup) return;

        if (scrollBar == null)
        {
            scrollBar = new GameObject().transform;
            scrollBar.name = "ScrollBar";
            scrollBar.SetParent(transform.parent);
            scrollBar.localScale = Vector3.one;
            scrollBar.localPosition = Vector3.zero;
            scrollBar.localEulerAngles = Vector3.zero;
            scrollBar.gameObject.AddComponent<ScrollBar>();
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
        setup = true;
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

    #region old executeAdd
    //private void executeAdd()
    //{
    //    if (objects == null) objects = new List<Transform>();
    //    int firstNewIndex = (objects.Count > 0) ? objects.Count : 0;


    //    foreach (Transform t in toAdd)
    //    {
    //        if (t == null)
    //        {
    //            toAdd.Clear();
    //            adding = false;
    //            return;
    //        }
    //        objects.Add(t);
    //    }


    //    setNumPagesAndHighestVisIndex();

    //    foreach (Transform t in toAdd)
    //    {
    //        placeObject(t, firstNewIndex, false);
    //        t.localScale = Vector3.one;
    //        ++firstNewIndex;
    //    }

    //    toAdd.Clear();
    //    adding = false;
    //}
    #endregion

    private void executeAdd()
    {
        int temp = atIndexAdd;

        foreach (Transform o in toAdd)
        {
            objects.Insert(temp, o);
            ++temp;
        }

        setNumPagesAndHighestVisIndex();

        temp = atIndexAdd;
        for (temp = atIndexAdd; temp < objects.Count; temp++)
        {
            placeObject(objects[temp], temp, false);
            objects[temp].localScale = Vector3.one;
        }

        toAdd.Clear();
        adding = false;
    }

    public int getScrollObjectCount()
    {
        if (objects == null) objects = new List<Transform>();
        return objects.Count;
    }

    //BUG: adding to beginning isn't accounted for, always added at the end (index is correct)
    public void addToScroll(List<Transform> objs, Transform obj, int atIndex)
    {
        if (objects == null) objects = new List<Transform>();

        if (atIndex < 0 || atIndex > objects.Count)
        {
            print("INDEX " + atIndex + " IS OUT OF RANGE OF SCROLL OBJECTS");
            return;
        }

        atIndexAdd = atIndex;

        if (objs != null && objs.Count > 0)
        {
            foreach (Transform o in objs)
            {
                o.localScale = Vector3.zero;
                toAdd.Add(o);
            }
        }
        else
        {
            obj.localScale = Vector3.zero;
            toAdd.Add(obj);
        }

        adding = true;
    }

    //old add implementation
    public void addObject(Transform newObj)
    {
        adding = true;

        if (newObj.localScale != Vector3.one)
        {
            print("MAKE THE SCALE OF THE TOP HIERARCHY OF THE SCROLL OBECT VECTOR3.ONE TO AVOID SCALING ISSUES");
        }

        newObj.localScale = Vector3.zero;
        toAdd.Add(newObj);
    }

    //old add implementation
    //public void addToIndex(int atIndex, List<Transform> objs, Transform obj, bool secondToLast)
    //{
    //    if (objects == null) objects = new List<Transform>();

    //    if (secondToLast) atIndex = objects.Count - 1;
    //    if (atIndex < 0 || atIndex > objects.Count)
    //    {
    //        return;
    //    }

    //    if (objs.Count > 0)
    //    {
    //        int temp = atIndex;
    //        foreach (Transform o in objs)
    //        {
    //            objects.Insert(temp, o);
    //            temp++;
    //        }
    //    }
    //    else
    //    {
    //        objects.Insert(atIndex, obj);
    //    }

    //    setNumPagesAndHighestVisIndex();

    //    for (int i = atIndex; i < objects.Count; i++)
    //    {
    //        placeObject(objects[i], i, false);
    //    }
    //}

    public int getIndex(Transform obj)
    {
        return objects.IndexOf(obj);
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
            if (deleting && !obj.gameObject.activeSelf) StartCoroutine(FadeButton(obj, false));
            obj.gameObject.SetActive(true);
        }
    }

    public void clear()
    {
        if (objects != null)
        {
            deleteObjects(objects);
        }
    }

    //TODO: account for deleting a single object
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
                        StartCoroutine(FadeButton(obj, true));
                    }
                    else if (i > highestVisIndex && i <= highestVisIndex + fixedRowOrCol)
                    {
                        StartCoroutine(FadeButton(obj, false));
                    }
                }
                else if (currDirection == direction.DOWN || currDirection == direction.RIGHT)
                {
                    if (i < lowestVisIndex && i >= lowestVisIndex - fixedRowOrCol)
                    {
                        StartCoroutine(FadeButton(obj, false));
                    }
                    else if (i <= highestVisIndex && i > highestVisIndex - fixedRowOrCol)
                    {
                        if (((highestVisIndex + 1) % fixedRowOrCol == 0) ||
                            (i > (highestVisIndex - ((highestVisIndex + 1) % fixedRowOrCol))))
                            StartCoroutine(FadeButton(obj, true));
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

    #region Coroutines
    IEnumerator FadeButton(Transform obj, bool fadeOut)
    {
        if (!fadeOut)
        {
            yield return new WaitForSeconds(fadeInDelay);
            obj.gameObject.SetActive(true);
        }

        float to = (fadeOut) ? transparent : opaque;
        Renderer[] childrenRenderer = obj.GetComponentsInChildren<Renderer>(true);

        foreach (Renderer r in childrenRenderer)
        {
            float from = (r.gameObject.GetComponent<TMPro.TextMeshPro>()) ?
                          r.material.GetColor("_FaceColor").a :
                          r.material.GetColor("_Color").a;
            StartCoroutine(FadeObj(r, obj, from, to));
        }

        yield return null;
    }

    IEnumerator FadeObj(Renderer rend, Transform rootParent, float start, float end)
    {
        Material mat = rend.material;
        string colorName = "_Color";
        Color col = Color.white;

        if (mat.HasProperty("_FaceColor")) colorName = "_FaceColor";
        if (mat.HasProperty(colorName)) col = mat.GetColor(colorName);

        for (float i = 0.0f; i < 1.0f; i += Time.deltaTime * (1 / fadeSpeed))
        {
            col = mat.GetColor(colorName);
            col.a = Mathf.Lerp(start, end, i);
            rend.material.SetColor(colorName, col);
            yield return null;
        }

        col.a = end;
        rend.GetComponent<Renderer>().material.SetColor(colorName, col);
        fading = false;

        if (end == transparent) rootParent.gameObject.SetActive(false);
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
