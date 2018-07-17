using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PieceWiseControl : MonoBehaviour
{

    CustomParametrizedSurface paramSurface;
    public GameObject plusTab;
    public GameObject firstTab;
    public FlexPanelComponent tabsPanel;
    // edit to change max number of tabs.
    int maxTabs = 10;
    private List<GameObject> tabList = new List<GameObject>(10);
    private GameObject currTab;

    CalcManager calcManager;

    GameObject saveTab;

    public PieceWiseControl Initialize(CalcManager cm)
    {
        this.calcManager = cm;
        this.paramSurface = cm.paramSurface;
        paramSurface.CreateExpressionSet();
        //calcManager.ChangeExpressionSet(paramSurface.expressionSets[0]);

        responder = new KeyboardInputResponder(this);
        menu.RegisterResponder(responder);
        //create saved copy of first tab
        saveTab = Instantiate(firstTab, firstTab.transform.position, firstTab.transform.rotation, firstTab.transform.parent);
        saveTab.transform.localScale = firstTab.transform.localScale;
        saveTab.SetActive(false);
        //find distance between plus and 1 for use in future spacing
        distanceBetween = plusTab.transform.localPosition - firstTab.transform.localPosition;

        tabList.Add(firstTab);
        SwitchToTab(firstTab);

        return this;
    }

    Vector3 distanceBetween;

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        PieceWiseControl pwControl;
        internal KeyboardInputResponder(PieceWiseControl pwControl)
        {
            this.pwControl = pwControl;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            pwControl.HandleInput(sender);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }

    }

    KeyboardInputResponder responder;
    public FlexMenu menu;

    protected void HandleInput(FlexActionableComponent sender)
    {
        switch (sender.name)
        {
            case "+":
                paramSurface.CreateExpressionSet();
                AddTab();
                break;
            case "delete":
                DeleteTab(currTab.name);
                break;
            case "clear":
                ClearAll();
                break;
            default:
                SwitchToTab(sender.gameObject);
                break;
        }
    }

    void AddTab()
    {
        GameObject g = AddPieceWise();
        g.transform.position = plusTab.transform.position;
        SwitchToTab(g);
        ShiftRight(plusTab);
    }

    int tabIndex = 1;

    void ShiftRight(GameObject g)
    {
        g.transform.localPosition += distanceBetween;
    }

    GameObject AddPieceWise()
    {
        // create new tab using first tab as a prefab.
        string name = "" + ++tabIndex;
        GameObject newTab = Instantiate(saveTab, saveTab.transform.position, saveTab.transform.rotation, saveTab.transform.parent);
        newTab.transform.localScale = saveTab.transform.localScale;
        newTab.GetComponentInChildren<TMPro.TextMeshPro>().text = name;
        newTab.name = name;
        newTab.SetActive(true);
        tabsPanel.AddAction(newTab.GetComponent<FlexActionableComponent>());
        // add it to dictionary of tabs using name as key
        tabList.Add(newTab);
        return newTab;
    }

    void DeleteTab(string tabName)
    {
        int i = tabList.FindIndex(x => x.name == tabName);
        GameObject g = tabList[i];
        tabList.RemoveAt(i);
        paramSurface.RemoveExpressionSet(i);
        if (tabList.Count != 0)
        {
            ShiftTab(plusTab);
        }
        ShiftTabsFromIndex(i);
        tabsPanel.RemoveAction(g.GetComponent<FlexActionableComponent>());
        Destroy(g);
        EnsureOne();
        SwitchToTab(tabList[System.Math.Min(i, tabList.Count - 1)]);
        calcManager.inputReceived = true;
    }

    void SwitchToTab(GameObject tab)
    {
        if (currTab != null)
        {
            currTab.GetComponent<FlexActionableComponent>().SetState(0);
        }
        currTab = tab;
        currTab.GetComponent<FlexActionableComponent>().SetState(2);
        int index = tabList.FindIndex(x => x.name == tab.name);
        index = System.Math.Min(index, paramSurface.expressionSets.Count - 1);
        calcManager.ChangeExpressionSet(paramSurface.expressionSets[index]);
    }

    void ClearAll()
    {
        foreach (GameObject g in tabList)
        {
            tabsPanel.RemoveAction(g.GetComponent<FlexActionableComponent>());
            Destroy(g);
        }
        tabList.Clear();
        EnsureOne();
    }

    void ShiftTabsFromIndex(int i)
    {
        List<GameObject> temp = tabList.GetRange(i, tabList.Count - i);
        StartCoroutine(ShiftAllTabs(temp));
        foreach (GameObject g in temp)
        {
            g.name = "" + (System.Convert.ToInt32(g.name) - 1);
            g.GetComponentInChildren<TMPro.TextMeshPro>().text = g.name;
        }
        --tabIndex;
    }

    void ShiftTab(GameObject g)
    {
        StartCoroutine(ShiftOneTab(g));
    }
    float totalTime = .2f;
    int numSteps = 20;
    IEnumerator ShiftAllTabs(List<GameObject> tabs)
    {
        float time = 0;
        Vector3 DistPerStep = distanceBetween / numSteps;
        while (time < totalTime)
        {
            time += totalTime / numSteps;
            foreach (GameObject tab in tabs)
            {
                if (tab != null)
                {
                    tab.transform.localPosition -= DistPerStep;
                }
            }
            yield return new WaitForSeconds(totalTime / numSteps);
        }
    }
    IEnumerator ShiftOneTab(GameObject tab)
    {
        float time = 0;
        Vector3 DistPerStep = distanceBetween / numSteps;
        while (time < totalTime)
        {
            time += totalTime / numSteps;
            tab.transform.localPosition -= DistPerStep;
            yield return new WaitForSeconds(totalTime / numSteps);
        }
    }

    void EnsureOne()
    {
        if (tabList.Count == 0)
        {
            tabIndex = 0;
            AddPieceWise();
        }
    }

    public void ForceNumberOfTabs(int n)
    {
        while (tabList.Count < n)
        {
            AddTab();
        }
        while (tabList.Count > n)
        {
            DeleteTab(currTab.name);
        }
    }

    // Update is called once per frame
    void Update()
    {
        plusTab.SetActive(tabList.Count < maxTabs);
    }
}