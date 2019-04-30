using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;

public class SaveLoadMenu : MonoBehaviour
{
    [Serializable]
    internal class ColorSettings
    {
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
    private CalcManager calcManager;
    private MultiSelectFlexPanel selectPanel;
    Dictionary<string, SavePackage> saves;

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

    private Scroll scroll;
    const int maxTextLength = 8;

    JoyStickAggregator joyStickAggregator;
    FlexMenu flexMenu;

    public void Initialize(CalcManager calcManager)
    {
        this.calcManager = calcManager;

        scroll = GetComponentInChildren<Scroll>(true);
        flexMenu = GetComponent<FlexMenu>();
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        flexMenu.RegisterResponder(responder);
        selectPanel = GetComponentInChildren<MultiSelectFlexPanel>().Initialize();
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();

        LoadFiles();
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
                    foreach (string toDelete in deleteList.Keys)
                    {
                        GameObject deleteObj = deleteList[toDelete];
                        loader.DeleteSave(toDelete);
                        saves.Remove(toDelete);
                        selectPanel.RemoveAction(deleteObj.GetComponent<FlexActionableComponent>());
                    }
                    scroll.deleteObjects(deleteList.Values.Select(x => x.transform).ToList());
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
        }
        else
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

        // @stats
        // save load panel
        Calcflow.UserStatistics.StatisticsTracking.InstantEvent("Save Load", source.name,
        new Dictionary<string, object>()
        {
            {"Deletion", delete}
        });
    }

    private void LoadFiles()
    {
        saves = loader.LoadExpressions();

        List<Transform> toAdd = new List<Transform>();
        foreach (SavePackage save in saves.Values)
        {
            GameObject button = createButton(save);
            button.SetActive(false);
            selectPanel.AddAction(button.GetComponent<FlexButtonComponent>());
        }
    }

    private void LoadNewFile(SavePackage save)
    {
        GameObject button = createButton(save);
        button.SetActive(false);

        FlexButtonComponent action = button.GetComponent<FlexButtonComponent>();
        action.selectedColor = (delete) ? colorSettings.deleteColor : colorSettings.selectColor;
        action.hoveringColor = (delete) ? colorSettings.deleteHoverColor : colorSettings.selectHoverColor;
        selectPanel.AddAction((FlexActionableComponent)action);

        if(!saves.ContainsKey(save.date))
        {
            saves.Add(save.date, save);
        }
    }

    private GameObject createButton(SavePackage save)
    {
        GameObject button = Instantiate(Resources.Load("Screenshot", typeof(GameObject))) as GameObject;
        button.name = save.date;
        // Parent the button's transform
        Transform panelTransform = transform.Find("Panel");
        button.transform.SetParent(panelTransform);

        initializeButton(save, button);
        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        return button;
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
            GameObject button = Instantiate(Resources.Load("Screenshot", typeof(GameObject))) as GameObject;
            scroll.addObject(button.transform);
            button.name = button.name + (ind + 1);
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

    public void Save()
    {
        loader.SaveDefault();
    }

    void Update() { }
}
