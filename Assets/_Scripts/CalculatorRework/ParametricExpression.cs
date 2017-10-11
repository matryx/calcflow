using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParametricExpression : MonoBehaviour {
    Expressions.ExpressionType type;
    List<Transform> expressions;
    List<Transform> variables;

    void Awake () {
        type = Expressions.ExpressionType.Paramet;
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
