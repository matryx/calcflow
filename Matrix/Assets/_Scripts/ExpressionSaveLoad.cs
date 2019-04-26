using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;
using Calcflow.UserStatistics;

public class ExpressionSaveLoad : MonoBehaviour
{
    const string jsonExtension = ".json";
    const string imageExtension = ".png";

    public SaveIconGenerator saveIconGenerator;
    public CustomParametrizedSurface customParametrizedSurface;

    public delegate void fileLoadedCallback(SavePackage save);
    public event fileLoadedCallback fileLoadedEvent;
    //string defaultFile;
    string savePath;
    string imagePath;

    public string SceneExtension;

    Dictionary<string, SavePackage> saves = new Dictionary<string, SavePackage>();
    // Use this for initialization
    void Start()
    {
        if (!initialized) Initialize();
    }

    private bool initialized = false; 
    void Initialize()
    {
        savePath = Path.Combine(Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "Calcflow"), "Saves");
        imagePath = Path.Combine(Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "Calcflow"), "Images");

        if (!Directory.Exists(savePath))
        {
            Directory.CreateDirectory(savePath);
        }
        if (!Directory.Exists(imagePath))
        {
            Directory.CreateDirectory(imagePath);
        }
        savePath = Path.Combine(savePath, SceneExtension);
        imagePath = Path.Combine(imagePath, SceneExtension);
        if (!Directory.Exists(savePath))
        {
            Directory.CreateDirectory(savePath);
        }
        if (!Directory.Exists(imagePath))
        {
            Directory.CreateDirectory(imagePath);
        }
    }

    public void SaveDefault()
    {
        SaveExpression(customParametrizedSurface.expressionSets);
    }

    public void SaveExpression(List<ExpressionSet> expressions)
    {
        string defaultFile = FileFriendly(DateTime.Now.ToString("dd/MM/yy/HH:mm:ss"));
        SaveExpression(expressions, defaultFile);
    }

    public static string FileFriendly(string input)
    {
        return input.Replace("/", "-").Replace(":", "_");
    }

    void SaveExpression(List<ExpressionSet> expressions, string fileName)
    {
        //converts the list of expressions into a serializable form.
        List<SerializableExpressionSet> sesList = expressions.Select(x => new SerializableExpressionSet(x)).ToList();
        string json = JsonHelper.ToJson(sesList);
        System.IO.File.WriteAllText(Path.Combine(savePath, fileName) + jsonExtension, json);

        if (saveIconGenerator != null)
        {
            saveIconGenerator.TakeScreenshot(Path.Combine(imagePath, fileName));
        }
        else
        {
            print("icon generator is null. No Icon will be generated");
        }
        SavePackage package = CreatePackageFromFile(fileName);
        saves.Add(package.date, package);
        if (fileLoadedEvent != null)
        {
            fileLoadedEvent.Invoke(package);
        }
    }

    void ReadExpressionsFromFile()
    {
        if (!initialized) Initialize();

        string[] files = Directory.GetFiles(savePath);

        foreach (string file in files)
        {
            SavePackage package = CreatePackageFromFile(file);
            saves.Add(package.date, package);
            if (fileLoadedEvent != null)
            {
                fileLoadedEvent.Invoke(package);
            }
        }
    }

    public void DeleteSave(string SaveName)
    {
        SaveName = FileFriendly(SaveName);
        File.Delete(Path.Combine(savePath, SaveName + jsonExtension));
        File.Delete(Path.Combine(imagePath, SaveName + imageExtension));

    }

    SavePackage CreatePackageFromFile(string file)
    {
        string name = System.IO.Path.GetFileNameWithoutExtension(file);
        string image = Path.Combine(imagePath, name + imageExtension);
        string save = Path.Combine(savePath, name + jsonExtension);
        List<ExpressionSet> ess = JsonHelper.FromJson<SerializableExpressionSet>(File.ReadAllText(save)).Select(x => x.ConvertToExpressionSet()).ToList();
        SavePackage package = new SavePackage(name.Replace("-", "/").Replace("_", ":"), image, ess);
        return package;
    }

    public Dictionary<string, SavePackage> LoadExpressions()
    {
        ReadExpressionsFromFile();
        return saves;
    }

    bool saveInput = false;
    bool SaveInput
    {
        get
        {
            return saveInput;
        }
        set
        {
            if (!saveInput && value)
            {
                SaveDefault();
            }
            saveInput = value;
        }
    }

    // Update is called once per frame
    void Update()
    {
        SaveInput = ((Input.GetKey(KeyCode.RightControl) || Input.GetKey(KeyCode.LeftControl)) && Input.GetKeyDown(KeyCode.S));
    }
}

public static class JsonHelper
{
    public static List<T> FromJson<T>(string json)
    {
        Wrapper wrapper = JsonUtility.FromJson<Wrapper>(json);
        return new List<T>(wrapper.Items.Select(i => JsonUtility.FromJson<T>(i)).ToList());
    }

    public static string ToJson<T>(List<T> list)
    {
        Wrapper wrapper = new Wrapper();
        wrapper.Items = list.Select(t => JsonUtility.ToJson(t)).ToArray();
        Debug.Log(JsonUtility.ToJson(wrapper));
        return JsonUtility.ToJson(wrapper);
    }

    [Serializable]
    private class Wrapper
    {
        public string[] Items;
    }
}

public class SavePackage
{
    public string imageFile;
    public string date;
    public List<ExpressionSet> expressionSetSet;

    public SavePackage (string date, string imageFile, List<ExpressionSet> expressionSetSet)
    {
        this.imageFile = imageFile;
        this.date = date;
        this.expressionSetSet = expressionSetSet;
    }

    public SavePackage() { }
}