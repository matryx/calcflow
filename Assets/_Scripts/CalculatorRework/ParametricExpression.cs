using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParametricExpression : MonoBehaviour
{
    Expressions.ExpressionType type;
    ExpressionSet expSet;
    List<Transform> expressions;
    List<Transform> variableClumps;
    List<Transform> variables;
    List<Transform> emptyList;
    Scroll scroll;
    bool initialized = false;

    void Awake()
    {
        if (initialized) return;
        type = Expressions.ExpressionType.Paramet;
        expSet = new ExpressionSet();
        expressions = new List<Transform>();
        variableClumps = new List<Transform>();
        variables = new List<Transform>();
        emptyList = new List<Transform>();

        scroll = FindObjectOfType<Scroll>();
        initialized = true;
    }

    public void Initialize()
    {
        if (!initialized)
        {
            type = Expressions.ExpressionType.Paramet;
            expressions = new List<Transform>();
            variableClumps = new List<Transform>();
            initialized = true;
        }
    }

    public string getVarTitle(Transform var)
    {
        if (variables.Contains(var))
        {
            int i = 0;
            foreach(Transform v in variables)
            {
                if (v.Equals(var))
                {
                    return variables[i].Find("VariableTitle").GetComponentInChildren<TMPro.TextMeshPro>().text;
                }
                i++;
            }
        }
        return "";
    }

    public ExpressionSet getExpSet()
    {
        return expSet;
    }

    public void addExpression(Transform expr)
    {
        expressions.Add(expr);
    }

    //NOTE: when you create a variable, make sure to add forwarder
    //      just like it's done in ExpressionSelector
    public void addVariable(Transform newVar)
    {
        if (variables.Count % 2 == 0)
        {
            addNewVariableClump(newVar);
        }
        else
        {
            addToVarClump(newVar);
        }

        variables.Add(newVar);
    }

    public Expressions.ExpressionType getType()
    {
        return type;
    }

    private void addToVarClump(Transform var)
    {
        var.SetParent(variableClumps[variableClumps.Count - 1]);
        var.localPosition = new Vector3(1.2f, 0, 0);
    }

    private void addNewVariableClump(Transform var)
    {
        int lastComponentInd = (variableClumps.Count > 0) ?
                           scroll.getIndex(variableClumps[variableClumps.Count - 1]) : scroll.getIndex(expressions[2]);

        Transform newVarClump = new GameObject().transform;
        newVarClump.name = "Var Clump";
        newVarClump.localScale = Vector3.one;
        newVarClump.localPosition = Vector3.zero;
        newVarClump.localEulerAngles = Vector3.zero;
        scroll.addToIndex(lastComponentInd + 1, emptyList, newVarClump, false);
        variableClumps.Add(newVarClump);

        var.SetParent(newVarClump);
        var.localPosition = new Vector3(-1.2f, 0, 0);
    }

    void Update()
    {

    }
}
