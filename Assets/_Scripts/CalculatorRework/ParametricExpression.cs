using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParametricExpression : MonoBehaviour {
    Expressions.ExpressionType type;
    List<Transform> expressions;
    List<Transform> variables;
    bool initialized = false;

    void Awake () {
        if (initialized) return;
        type = Expressions.ExpressionType.Paramet;
        expressions = new List<Transform>();
        variables = new List<Transform>();
        initialized = true;
    }

    public void Initialize()
    {
        if (!initialized)
        {
            type = Expressions.ExpressionType.Paramet;
            expressions = new List<Transform>();
            variables = new List<Transform>();
            initialized = true;
        }
    }

    public void addExpression(Transform expr)
    {
        expressions.Add(expr);
    }

    public void addVariable(Transform var)
    {
        variables.Add(var);
    }

    void Update () {
		
	}
}
