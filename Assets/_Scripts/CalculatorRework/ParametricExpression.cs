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
    List<Transform> variables;
    List<Transform> emptyList;
    Scroll scroll;
    bool initialized = false;

    void Awake()
    {
        if (initialized) return;
        type = Expressions.ExpressionType.Paramet;
        expressionsClass = Expressions._instance;
        expSet = new ExpressionSet();
        expressionsList = new List<Transform>();
        variableClumps = new List<Transform>();
        variables = new List<Transform>();
        emptyList = new List<Transform>();

        scroll = expressionsClass.getParametricScroll();
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
        expressionsList.Add(expr);
    }

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

    //TODO: handle deletion of variables and re-arranging that will happen as a result

    private void addToVarClump(Transform var)
    {
        var.SetParent(variableClumps[variableClumps.Count - 1]);
        var.localPosition = new Vector3(1.2f, 0, 0);
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
        scroll.addToIndex(lastComponentInd + 1, emptyList, newVarClump, false);
        variableClumps.Add(newVarClump);

        var.SetParent(newVarClump);
        var.localPosition = new Vector3(-1.2f, 0, 0);
    }

    void Update()
    {

    }
}
