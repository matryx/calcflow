using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParametricExpression : MonoBehaviour
{
    Expressions.ExpressionType type;
    Expressions expressionsClass;
    ExpressionSet expSet;
    List<Transform> expressionsList;
    List<Transform> variableClumps;
    Dictionary<string, Transform> variables;
    List<Transform> emptyList;
    Transform separator;
    Scroll scroll;
    bool initialized = false;
    float xPos = 1.2f;

    void Awake()
    {
        if (initialized) return;
        type = Expressions.ExpressionType.Paramet;
        expressionsClass = Expressions._instance;
        expSet = new ExpressionSet();
        expressionsList = new List<Transform>();
        variableClumps = new List<Transform>();
        variables = new Dictionary<string, Transform>();
        emptyList = new List<Transform>();

        scroll = expressionsClass.getScroll("param");
        initialized = true;

    }

    public void Initialize()
    {
        if (!initialized)
        {
            type = Expressions.ExpressionType.Paramet;
            expressionsList = new List<Transform>();
            variableClumps = new List<Transform>();
            initialized = true;
        }
    }

    public Scroll getScroll()
    {
        return scroll;
    }

    public void setButtonInputMaterial(Material mat)
    {
        foreach (Transform t in expressionsList)
        {
            t.Find("Button_Input").GetComponent<Renderer>().material = mat;
        }
    }

    public void setElementQuadtex(Texture tex)
    {
        foreach (KeyValuePair<string, Transform> t in variables)
        {
            t.Value.GetChild(0).Find("Quad").GetComponent<Renderer>().material.SetTexture("_MainTex", tex);
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

    //public string getVarTitle(Transform var)
    //{
    //    if (variables.ContainsValue(var))
    //    {
    //        foreach (KeyValuePair<string, Transform> v in variables)
    //        {
    //            if (v.Value.Equals(var))
    //            {
    //                return v.Value.Find("VariableTitle").GetComponentInChildren<TMPro.TextMeshPro>().text;
    //            }
    //        }
    //    }
    //    return "";
    //}

    public void setSeparator(Transform sep)
    {
        separator = sep;
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

    public Expressions.ExpressionType getType()
    {
        return type;
    }

    //TODO: fix the fade in from scroll (fading in too early, when it's still out of the board's dimensions
    //      - make it so that objects don't start fading in until they're inside the board's dimensions
    public void deleteExpressionFromScroll()
    {
        variableClumps.Add(separator);
        scroll.deleteObjects(variableClumps);
        scroll.deleteObjects(expressionsList);
    }

    //BUG: doesn't rearrange properly or remove clumps when it should
    public void deleteVariable(string varToDelete)
    {
        Destroy(variables[varToDelete].gameObject);
        variables.Remove(varToDelete);
        Transform del = null;

        for (int i = 0; i < variableClumps.Count; i++)
        {
            if (variableClumps[i].childCount != 2)
            {
                //TODO: move to coroutine
                variableClumps[i].GetChild(0).localPosition = new Vector3(-xPos, 0, 0);

                if (i + 1 < variableClumps.Count)
                {
                    variableClumps[i + 1].GetChild(0).SetParent(variableClumps[i]);
                    variableClumps[i + 1].GetChild(0).localPosition = new Vector3(xPos, 0, 0);
                }

                if (variableClumps[i].childCount == 0) del = variableClumps[i];
            }
        }

        if (del != null)
        {
            variableClumps.Remove(del);
            List<Transform> temp = new List<Transform>();
            temp.Add(del);
            scroll.deleteObjects(temp);
        }
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

    private void addToVarClump(Transform var)
    {
        var.SetParent(variableClumps[variableClumps.Count - 1]);
        var.localPosition = new Vector3(xPos, 0, 0);
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
        //scroll.addToIndex(lastComponentInd + 1, emptyList, newVarClump, false);
        scroll.addToScroll(null, newVarClump, lastComponentInd + 1);
        variableClumps.Add(newVarClump);

        var.SetParent(newVarClump);
        var.localPosition = new Vector3(-xPos, 0, 0);
    }

    void Update()
    {

    }
}
