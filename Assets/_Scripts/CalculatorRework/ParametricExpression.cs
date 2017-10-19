using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParametricExpression : MonoBehaviour
{
    Expressions.ExpressionType type;
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
        expressions = new List<Transform>();
        variableClumps = new List<Transform>();
        variables = new List<Transform>();
        emptyList = new List<Transform>();

        scroll = transform.parent.parent.GetComponentInChildren<Scroll>();
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

    public void addExpression(Transform expr)
    {
        expressions.Add(expr);
    }

    //TODO: test
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
        newVarClump.localScale = Vector3.one;
        newVarClump.localPosition = Vector3.zero;
        newVarClump.localEulerAngles = Vector3.zero;
        scroll.addToIndex(lastComponentInd + 1, emptyList, newVarClump, false);
        variableClumps.Add(var);

        var.SetParent(newVarClump);
        var.localPosition = new Vector3(-1.2f, 0, 0);
    }

    void Update()
    {

    }
}
