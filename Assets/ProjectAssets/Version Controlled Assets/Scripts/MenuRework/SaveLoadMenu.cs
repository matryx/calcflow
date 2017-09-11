using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;

public class SaveLoadMenu : MonoBehaviour
{
    [Serializable]
    internal class ColorSettings {
        [Header("Select Colors")]
        public Color selectColor;
        public Color deleteColor;
        [Header("Hover Colors")]
        public Color deleteHoverColor;
        public Color selectHoverColor;
    }
    [SerializeField]
    private ColorSettings colorSettings;


    private List<Transform> buttons = new List<Transform>();
    public ExpressionSaveLoad loader;
    Dictionary<string, SavePackage> saves;
    private CalcManager calcManager;

    private MultiSelectFlexPanel selectPanel;


    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        SaveLoadMenu saveLoadMenu;
        internal KeyboardInputResponder(SaveLoadMenu saveLoadMenu)
        {
            this.saveLoadMenu = saveLoadMenu;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            saveLoadMenu.HandleInput(sender.gameObject);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }

    }

    #region scrolling/fading
    private List<Transform> fadingButtons = new List<Transform>();
    private List<Transform> targetOpaqueButtons = new List<Transform>();

    bool moving = false;
    bool fadingIn = false;
    bool fadingOut = false;

    float xPos, yPos = 0;
    const float yOffset = 1.6f;
    const float opaque = 1, transparent = 0;

    const int perRow = 2;
    const int numVis = 6;
    const int maxTextLength = 8;
    int direction = 1;
    const int up = 1;
    const int down = 0;
    int lowestVisIndex, highestVisIndex;

    Vector3 toPos;
    Transform panel;
    PageIndicator pageInd;

    IEnumerator fadeIn, fadeOut;
    #endregion

    JoyStickAggregator joyStickAggregator;
    FlexMenu flexMenu;

    // Use this for initialization
    public void Initialize(CalcManager calcManager)
    {
        this.calcManager = calcManager;

        flexMenu = GetComponent<FlexMenu>();
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        flexMenu.RegisterResponder(responder);
        selectPanel = GetComponentInChildren<MultiSelectFlexPanel>();
        joyStickAggregator = GetComponent<JoyStickAggregator>();

        panel = selectPanel.transform;
        pageInd = GetComponentInChildren<PageIndicator>().Initialize();

        GetComponent<FlexPanelComponent>();

        LoadFiles();

        lowestVisIndex = 0;
        highestVisIndex = (buttons.Count >= numVis) ? (numVis - 1) : (buttons.Count - 1);
        int numPages = (highestVisIndex == buttons.Count - 1) ?
                    1 : 1 + (int)System.Math.Ceiling((double)(buttons.Count - numVis) / perRow);

        pageInd.setNumPages(numPages);
        loader.fileLoadedEvent += LoadNewFile;
    }
    private bool delete = false;
    private Dictionary<string, GameObject> deleteList = new Dictionary<string, GameObject>();
    private void HandleInput(GameObject source)
    {
        string name = source.name;
        if (delete)
        {
            switch (name)
            {
                case "DeleteSelected":
                    bool deletedStuff = false;
                    foreach (string toDelete in deleteList.Keys)
                    {
                        deletedStuff = true;
                        GameObject deleteObj = deleteList[toDelete];
                        buttons.Remove(deleteObj.transform);
                        loader.DeleteSave(toDelete);
                        saves.Remove(toDelete);
                        GameObject.Destroy(deleteObj);
                    }
                    //stop from rearranging and scrolling up if nothing was actually deleted.
                    if (deletedStuff)
                    {
                        ArrangeButtons(buttons);
                        int numPages = 1 + (int)System.Math.Ceiling((double)(buttons.Count - numVis) / perRow);
                        numPages = (numPages > 0) ? numPages : 1;
                        pageInd.setNumPages(numPages);
                        pageInd.setCurrentPage(1);
                    }
                    //deleteList.Clear(); // removed because redundant with fallthrough to delete;
                    goto case "delete";
                case "delete":
                    deleteList.Clear();
                    selectPanel.SwitchToSingleSelect();
                    selectPanel.ChangeHoveringColor(colorSettings.selectHoverColor);
                    selectPanel.ChangeSelectedColor(colorSettings.selectColor);
                    delete = false;
                    break;
                default:
                    if (deleteList.ContainsKey(name))
                    {
                        deleteList.Remove(name);
                    }
                    else
                    {
                        deleteList.Add(name, source);
                    }
                    break;
            }

        } else
        {
            switch (name)
            {
                case "delete":
                    selectPanel.SwitchToMultiSelect();
                    selectPanel.ChangeHoveringColor(colorSettings.deleteHoverColor);
                    selectPanel.ChangeSelectedColor(colorSettings.deleteColor);
                    delete = true;
                    break;
                default:
                    List<ExpressionSet> ess = saves[name].expressionSetSet;


                    calcManager.LoadSavedExpressionSets(ess);
                    break;
            }
        }
    }

    private void LoadFiles()
    {
        saves = loader.LoadExpressions();
        print("" + saves.Count + " files originally loaded");
        //saves.Values.Select(save => createButton(save));
        foreach(SavePackage save in saves.Values)
        {
            createButton(save);
        }
    }

    private void QueueToLoad(SavePackage save)
    {
        toLoad.Add(save);
    }

    private void LoadNewFile(SavePackage save)
    {
        GameObject button = createButton(save);
        print("additional file loaded");
        int numPages = 1 + (int)System.Math.Ceiling((double)(buttons.Count - numVis) / perRow);
        numPages = (numPages > 0) ? numPages : 1;
        pageInd.setNumPages(numPages);
        pageInd.setCurrentPage((lowestVisIndex/2) + 1);

        FlexButtonComponent action = button.GetComponent<FlexButtonComponent>();
        action.selectedColor = (delete) ? colorSettings.deleteColor : colorSettings.selectColor;
        action.hoveringColor = (delete) ? colorSettings.deleteHoverColor : colorSettings.selectHoverColor;
        selectPanel.AddAction((FlexActionableComponent)action);

        saves.Add(save.date, save);
    }

    private GameObject createButton(SavePackage save)
    {
        GameObject button =
            Instantiate(Resources.Load("Screenshot", typeof(GameObject))) as GameObject;
        button.transform.SetParent(panel);
        button.name = save.date;
        buttons.Add(button.transform);
        int ind = buttons.Count - 1;
        xPos = (ind % 2) == 0 ? 0 : 1.3f;
        yPos = ((ind % perRow) == 0 && (ind != 0)) ? (yPos - 1.6f) : yPos;
        if (buttons.Count == 1)
        {
            button.transform.localPosition = new Vector3(xPos, yPos, 0);
        }
        else
        {
            button.transform.localPosition = new Vector3(xPos, yPos + buttons.First().transform.localPosition.y, 0);
        }
        button.transform.localEulerAngles = Vector3.zero;

        initializeButton(save, button);


        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        if (((highestVisIndex == ind-1) && (ind % 2 == 1)) || (buttons.Count < numVis))
        {
            highestVisIndex = buttons.Count - 1;
        }

        if (ind > highestVisIndex) button.SetActive(false);

        return button;
    }

    private void ArrangeButtons(List<Transform> buttonList){
        xPos = 0;
        yPos = 0;

        lowestVisIndex = 0;
        highestVisIndex = Math.Min(buttonList.Count - 1, numVis - 1);

        for (int ind=0; ind < buttonList.Count; ind++)
        {
            Transform button = buttonList[ind];
            xPos = (ind % 2) == 0 ? 0 : 1.3f;
            yPos = ((ind % perRow) == 0 && (ind != 0)) ? (yPos - 1.6f) : yPos;
            //print("index " + ind + " " + xPos + " " + yPos);
            if (ind == 0)
            {
                button.transform.localPosition = new Vector3(xPos, yPos, 0);
            }
            else
            {
                button.transform.localPosition = new Vector3(xPos, yPos + buttonList[0].transform.localPosition.y, 0);
            }
            button.transform.localEulerAngles = Vector3.zero;
            button.gameObject.SetActive(ind <= highestVisIndex);
            button.gameObject.GetComponentInChildren<Collider>().enabled = (ind <= highestVisIndex);
            foreach (Renderer r in button.GetComponentsInChildren<Renderer>()) {
                Color c;
                if (r.material.HasProperty("_Color"))
                {
                    c = r.material.GetColor("_Color");
                    c.a = 1;
                    r.material.SetColor("_Color", c);
                } else if (r.material.HasProperty("_FaceColor"))
                {
                    c = r.material.GetColor("_FaceColor");
                    c.a = 1;
                    r.material.SetColor("_FaceColor", c);
                }
            }
        }
    }

    private void initializeButton(SavePackage save, GameObject button)
    {
        Texture2D imageTexture = LoadIMG(save.imageFile);
        string imgName = save.date;
        if (imgName.Length > maxTextLength) imgName = imgName.Replace(imgName.Substring(maxTextLength), "...");
        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = imgName;
        button.transform.Find("Img").GetComponent<Renderer>().material.mainTexture = imageTexture;
    }

    private void createButtons(string[] files)
    {
        for (int ind = 0; ind < files.Length; ind++)
        {
            GameObject button =
                Instantiate(Resources.Load("Screenshot", typeof(GameObject))) as GameObject;
            button.transform.SetParent(panel);
            button.name = button.name + (ind + 1);

            xPos = ((ind % 2) == 0) ? 0 : 1.3f;
            yPos = ((ind % perRow) == 0 && (ind != 0)) ? (yPos - 1.6f) : yPos;

            button.transform.localPosition = new Vector3(xPos, yPos, 0);
            button.transform.localEulerAngles = Vector3.zero;

            buttons.Add(button.transform);
            if (ind >= numVis) button.SetActive(false);
        }
    }

    private void initializeButtons(string[] files)
    {
        Texture2D t;
        string imgName;
        int startIndex;

        int i = 0;
        foreach (Transform child in GetComponentInChildren<SelectorFlexPanel>().transform)
        {
            if (child.GetComponent<VirtualButton>())
            {
                t = LoadIMG(files[i]);
                startIndex = files[i].LastIndexOf(@"\");
                imgName = files[i].Substring(startIndex + 1).Replace(@".jpg", "");
                if (imgName.Length > maxTextLength) imgName = imgName.Replace(imgName.Substring(maxTextLength), "...");
                child.Find("Text").GetComponent<TMPro.TextMeshPro>().text = imgName;
                child.Find("Img").GetComponent<Renderer>().material.mainTexture = t;
                i++;
            }
        }
    }

    private static Texture2D LoadIMG(string filePath)
    {
        Texture2D tex = null;
        byte[] fileData;

        if (File.Exists(filePath))
        {
            fileData = File.ReadAllBytes(filePath);
            tex = new Texture2D(2, 2);
            tex.LoadImage(fileData);
        }
        return tex;
    }

    #region fade/scroll

    public void moveButtons(int dir)
    {
        if ((dir != down && dir != up) ||
            (dir == down && lowestVisIndex == 0) ||
            (dir == up && highestVisIndex == buttons.Count - 1))
            return;

        direction = dir;

        Transform button;
        Vector3 newPos;
        float newY;

        if (!moving && !fadingIn && !fadingOut)
        {
            moving = true;
            fadingIn = true;
            fadingOut = true;
            fadingButtons.Clear();
            targetOpaqueButtons.Clear();

            //SMALL BUG: if button fading in and all of a sudden needs to fade out (joystick up then down)
            //           body shows on the tabs because fading in has that slight delay before animating
            for (int i = 0; i < buttons.Count; i++)
            {
                button = buttons[i];

                newY = (direction == up) ?
                        (float)System.Math.Round((double)button.localPosition.y + yOffset, 1) :
                        (float)System.Math.Round((double)button.localPosition.y - yOffset, 1);
                newPos = new Vector3(button.localPosition.x, newY, button.localPosition.z);

                if (i == 0)
                    toPos = newPos;

                if (direction == up)
                {
                    if (i == lowestVisIndex || i == lowestVisIndex + 1)
                    {
                        fadingButtons.Add(button);
                    }
                    else if (i == highestVisIndex + 1 || i == highestVisIndex + 2)
                    {
                        targetOpaqueButtons.Add(button);
                    }
                }
                else if (direction == down)
                {
                    if (i == lowestVisIndex - 1 || i == lowestVisIndex - 2)
                    {
                        targetOpaqueButtons.Add(button);
                    }
                    else if (i == highestVisIndex || (i == highestVisIndex - 1 && (highestVisIndex % 2) != 0))
                    {
                        fadingButtons.Add(button);
                    }
                }

                StartCoroutine(MoveTo(button, button.localPosition, newPos, 0.3f));
            }

            fadeIn = FadeButtonIn();
            fadeOut = FadeButtonOut();
            StartCoroutine(fadeIn);
            StartCoroutine(fadeOut);

            pageInd.moveScroller(direction);

            if (direction == up)
            {
                lowestVisIndex += perRow;
                highestVisIndex += perRow;

                highestVisIndex = (highestVisIndex == buttons.Count) ?
                                   highestVisIndex - 1 : highestVisIndex;
            }
            else
            {
                lowestVisIndex -= perRow;
                highestVisIndex = (highestVisIndex % 2 == 0) ?
                                   highestVisIndex - 1 : highestVisIndex - 2;
            }
        }
    }

    public void Save()
    {
        print("save default");
        loader.SaveDefault();
    }

    IEnumerator FadeButtonOut()
    {
        if (fadingButtons.Count > 0)
        {
            yield return StartCoroutine(AnimateFade(fadingButtons,
                fadingButtons[0].GetComponentInChildren<Renderer>().material.GetColor("_Color").a, transparent, 0.15f)); //0.15
        }

        yield return new WaitForSeconds(0.1f); //0.1

        foreach (Transform b in fadingButtons)
        {
            b.gameObject.SetActive(false);
        }

        fadingButtons.Clear();
        fadingOut = false;
    }

    IEnumerator FadeButtonIn()
    {
        yield return new WaitForSeconds(0.2f);     //0.2

        foreach (Transform b in targetOpaqueButtons)
        {
            b.gameObject.SetActive(true);
        }

        if (targetOpaqueButtons.Count > 0)
        {
            yield return StartCoroutine(AnimateFade(targetOpaqueButtons,
                targetOpaqueButtons[0].GetComponentInChildren<Renderer>().material.GetColor("_Color").a, opaque, 0.25f)); //0.25
        }

        targetOpaqueButtons.Clear();
        fadingIn = false;
    }

    IEnumerator AnimateFade(List<Transform> buttonsToFade, float from, float to, float time)
    {
        bool state;

        foreach (Transform b in buttonsToFade)
        {
            foreach (Transform child in b)
            {
                if (child.name == "Body")
                {
                    state = (to == transparent) ? false : true;
                    child.GetComponent<Collider>().enabled = state;
                }

                switch (child.name)
                {
                    case "Text":
                        StartCoroutine(FadeText(child, from, to, time));
                        break;
                    case "Img":
                        StartCoroutine(Fade(child, from, to, time));
                        break;
                    case "Body":
                        if (to == transparent)
                        {
                            Color col = child.GetComponent<Renderer>().material.GetColor("_Color");
                            col.a = to;
                            child.GetComponent<Renderer>().material.SetColor("_Color", col);
                        }
                        else
                        {
                            StartCoroutine(Fade(child, from, to, time));
                        }

                        break;
                }
            }
        }

        yield return null;
    }

    IEnumerator Fade(Transform obj, float start, float end, float time)
    {
        Material mat = obj.GetComponent<Renderer>().material;
        Color col = mat.GetColor("_Color");

        for (float i = 0.0f; i < 1.0f; i += Time.deltaTime * (1 / time))
        {
            col = mat.GetColor("_Color");
            col.a = Mathf.Lerp(start, end, i);
            obj.GetComponent<Renderer>().material.SetColor("_Color", col);

            yield return null;
        }

        col.a = end;
        obj.GetComponent<Renderer>().material.SetColor("_Color", col);
    }

    IEnumerator FadeText(Transform obj, float start, float end, float time)
    {
        Color col = obj.GetComponent<Renderer>().material.GetColor("_FaceColor");

        for (float i = 0.0f; i < 1.0f; i += Time.deltaTime * (1 / time))
        {
            col.a = Mathf.Lerp(start, end, i);
            obj.GetComponent<Renderer>().material.SetColor("_FaceColor", col);

            yield return null;
        }

        col.a = end;
        obj.GetComponent<Renderer>().material.SetColor("_FaceColor", col);
    }

    IEnumerator MoveTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            obj.localPosition = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        obj.localPosition = end;
    }

    List<SavePackage> toLoad = new List<SavePackage>();

    void Update()
    {
        if (moving && buttons[0].localPosition == toPos)
            moving = false;
        if (!moving && toLoad.Count > 0)
        {
            foreach (SavePackage savePackage in toLoad)
            {
                LoadNewFile(savePackage);
            }
            toLoad.Clear();
        }
    }
    #endregion

}
