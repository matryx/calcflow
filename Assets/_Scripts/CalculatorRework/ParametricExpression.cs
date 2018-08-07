using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParametricExpression : MonoBehaviour, ExpressionTabInterface
{
    Expressions expressionsClass;
    ExpressionSet expSet;
    ExpressionActions expActions;

    List<Transform> expressionsList;
    List<Transform> variableClumps;
    List<string> varsToDelete;

    Dictionary<string, Transform> variables;
    Dictionary<string, Transform> hiddenVariables;

    Transform expressionX;
    Transform separator;

    Scroll scroll;

    bool initialized = false;
    float xPos = 1.2f;
    bool deleteVar = false;
    bool destroyCalled = false;
    bool isActive = true;

    void Awake()
    {
        if (initialized) return;
        expressionsClass = Expressions._instance;
        expSet = new ExpressionSet();
        expressionsList = new List<Transform>();
        variableClumps = new List<Transform>();
        variables = new Dictionary<string, Transform>();
        hiddenVariables = new Dictionary<string, Transform>();
        varsToDelete = new List<string>();
        expActions = transform.GetChild(0).GetChild(0).GetChild(0).GetComponentInChildren<ExpressionActions>();

        scroll = expressionsClass.getScroll(Expressions.ExpressionType.PARAMET);
        initialized = true;
    }

    public void Initialize()
    {
        if (!initialized)
        {
            expressionsList = new List<Transform>();
            variableClumps = new List<Transform>();
            initialized = true;
        }
    }

    public ExpressionActions getExpActions()
    {
        return expActions;
    }

    public void setActiveStatus(bool status)
    {
        isActive = status;
    }

    public bool getActiveStatus()
    {
        return isActive;
    }

    public Scroll getScroll()
    {
        return scroll;
    }

    public void setButtonInputColor(Color col)
    {
        foreach (Transform t in expressionsList)
        {
            t.Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
        }

        foreach (KeyValuePair<string, Transform> t in variables)
        {
            t.Value.Find("Min").Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
            t.Value.Find("Max").Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
        }
    }

    public void setElementQuadTex(Texture tex)
    {
        foreach (KeyValuePair<string, Transform> t in variables)
        {
            t.Value.GetChild(0).Find("Quad").GetComponent<Renderer>().material.mainTexture = tex;
        }
    }

    public void setTextColor(Color c)
    {
        foreach (Transform t in expressionsList)
        {
            foreach (Transform child in t)
            {
                if (child.GetComponent<TMPro.TextMeshPro>())
                {
                    child.GetComponent<TMPro.TextMeshPro>().color = c;
                }
            }
        }

        foreach (KeyValuePair<string, Transform> t in variables)
        {
            foreach (Transform child in t.Value)
            {
                foreach (Transform gchild in child)
                {
                    if (gchild.GetComponent<TMPro.TextMeshPro>())
                    {
                        gchild.GetComponent<TMPro.TextMeshPro>().color = c;
                    }
                }
            }
        }
    }

    public void setExpressionX(Transform e)
    {
        expressionX = e;
    }

    public Transform getExpressionX()
    {
        return expressionX;
    }

    public void setSeparator(Transform sep)
    {
        separator = sep;
    }

    public Transform getSeparator()
    {
        return separator;
    }

    public ExpressionSet getExpSet()
    {
        return expSet;
    }

    public void addExpression(Transform expr)
    {
        expressionsList.Add(expr);
    }

    public void addVariable(string varName, Transform varValue)
    {
        if (hiddenVariables.ContainsKey(varName))
        {
            Transform temp = hiddenVariables[varName];
            temp.gameObject.SetActive(true);
            varValue = temp;
            hiddenVariables.Remove(varName);
        }

        if (variables.Count % 2 == 0)
        {
            addNewVariableClump(varValue);
        }
        else
        {
            addToVarClump(varValue);
        }

        variables.Add(varName, varValue);
    }

    public void deleteExpressionFromScroll()
    {
        scroll.deleteObjects(expressionsList);
        variableClumps.Add(separator);
        scroll.deleteObjects(variableClumps);
    }

    public void deleteVariable(List<string> vars)
    {
        varsToDelete = vars;
        deleteVar = true;
    }

    private void addToVarClump(Transform var)
    {
        var.SetParent(variableClumps[variableClumps.Count - 1]);
        var.localPosition = new Vector3(xPos, 0, 0);
        var.localScale = Vector3.one;
        var.localEulerAngles = Vector3.zero;
        var.gameObject.SetActive(true);
    }

    private void addNewVariableClump(Transform var)
    {
        int lastComponentInd = (variableClumps.Count > 0) ?
                           scroll.getIndex(variableClumps[variableClumps.Count - 1]) : scroll.getIndex(expressionsList[2]);

        Transform newVarClump = new GameObject().transform;
        newVarClump.name = "Var Clump";
        newVarClump.localScale = Vector3.one;
        newVarClump.localPosition = Vector3.zero;
        newVarClump.localEulerAngles = Vector3.zero;
        scroll.addToScroll(null, newVarClump, lastComponentInd + 1);
        variableClumps.Add(newVarClump);

        var.SetParent(newVarClump);
        var.localPosition = new Vector3(-xPos, 0, 0);
        var.localScale = Vector3.one;
        var.localEulerAngles = Vector3.zero;
        var.gameObject.SetActive(true);
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

    void Update()
    {
        if (deleteVar)
        {
            hideVariables();
            return;
        }

        bool noMoreSlots = false;

        if (destroyCalled)
        {
            for (int i = 0; i < variableClumps.Count; i++)
            {
                if (noMoreSlots) break;

                Transform currSlot = variableClumps[i];

                if (currSlot.childCount < 2)
                {
                    if (currSlot.childCount == 1)
                    {
                        StartCoroutine(MoveTo(currSlot.GetChild(0), currSlot.GetChild(0).localPosition, new Vector3(-xPos, 0, 0), 0.3f));
                    }
                    else
                    {
                        noMoreSlots = findNextSlot(currSlot, i, 0, -xPos, false);
                    }

                    noMoreSlots = findNextSlot(currSlot, i, 1, xPos, true);
                }
            }

            destroyEmptyClumps();
            destroyCalled = false;
        }
    }

    private void hideVariables()
    {
        foreach (string s in varsToDelete)
        {
            if (variables.ContainsKey(s))
            {
                Transform temp = variables[s];
                temp.gameObject.SetActive(false);
                temp.SetParent(null);
                hiddenVariables.Add(s, temp);
                variables.Remove(s);

                destroyCalled = true;
                deleteVar = false;
            }
        }
    }

    private bool findNextSlot(Transform currClump, int currClumpIndex, int childIndex, float xpos, bool checkNoSlot)
    {
        for (int ni = currClumpIndex + 1; ni < variableClumps.Count; ni++)
        {
            Transform nSlot = variableClumps[ni];

            if (nSlot.childCount > 0)
            {
                nSlot.GetChild(0).SetParent(currClump);
                StartCoroutine(MoveTo(currClump.GetChild(childIndex), currClump.GetChild(childIndex).localPosition, new Vector3(xpos, 0, 0), 0.3f));
                return false;
            }

            //only want to do check when finding a slot for second var spot
            if (checkNoSlot && ni == variableClumps.Count - 1) return true;
        }

        return false;
    }

    private void destroyEmptyClumps()
    {
        int removeFrom = 0;
        bool remove = false;

        for (int ind = 0; ind < variableClumps.Count; ind++)
        {
            if (variableClumps[ind].childCount == 0)
            {
                removeFrom = ind;
                remove = true;
                break;
            }
        }

        List<Transform> emptyClumps = new List<Transform>();

        if (remove)
        {
            emptyClumps = variableClumps.GetRange(removeFrom, variableClumps.Count - removeFrom);
            variableClumps.RemoveRange(removeFrom, variableClumps.Count - removeFrom);
            scroll.deleteObjects(emptyClumps);
        }
    }
}
