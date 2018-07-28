using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VariableShortcut : MonoBehaviour
{
    public static VariableShortcut _instance;
    public Dictionary<string, Transform> shortcuts = new Dictionary<string, Transform>();

    Scroll thisScroll;
    List<string> varsPressed = new List<string>();
    Transform buttonToRemove;

    int maxLength = 5;

    void Start()
    {
        _instance = this;
        thisScroll = GetComponentInChildren<Scroll>();
    }

    private void addVarShortcut(string varName)
    {
        if (shortcuts.Count == maxLength) removeVarShortcut();

        GameObject varButton = Instantiate(Resources.Load("Prefabs/VarShortcut", typeof(GameObject))) as GameObject;
        varButton.transform.GetComponentInChildren<TMPro.TextMeshPro>().text = varName;
        varButton.name = varName;
        shortcuts.Add(varName, varButton.transform);
        int objCount = thisScroll.getScrollObjectCount();
        int atIndex = (objCount > 0) ? objCount : 0;
        thisScroll.addToScroll(null, varButton.transform, atIndex);
        transform.GetComponent<KeyboardFlexPanel>().AddAction(varButton.transform.GetComponent<FlexActionableComponent>());
    }

    public void recordVarPress(string var)
    {
        if (varsPressed.Contains(var))
        {
            varsPressed.Remove(var);
        }
        else
        {
            addVarShortcut(var);
        }

        varsPressed.Add(var);
    }

    public void removeVarShortcut()
    {
        string[] tempArray = varsPressed.ToArray();
        string varToRemove = tempArray[0];

        shortcuts.TryGetValue(varToRemove, out buttonToRemove);
        List<Transform> removeList = new List<Transform>();
        removeList.Add(buttonToRemove);

        transform.GetComponent<KeyboardFlexPanel>().RemoveAction(buttonToRemove.GetComponent<FlexActionableComponent>());
        varsPressed.Remove(varToRemove);
        shortcuts.Remove(varToRemove);
        thisScroll.deleteObjects(removeList);
    }

    void Update() { }
}
