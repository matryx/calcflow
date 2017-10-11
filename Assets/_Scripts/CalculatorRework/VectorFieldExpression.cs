using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VectorFieldExpression : MonoBehaviour {
    Expressions.ExpressionType type;
    List<Transform> expressions;
    Transform range;

    void Awake () {
        type = Expressions.ExpressionType.VecField;
    }

    public void addExpression(Transform expr)
    {
        expressions.Add(expr);
    }

    public void setRange(Transform ran)
    {
        range = ran;
    }

    void Update () {
		
	}
}
