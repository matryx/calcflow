using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VectorFieldExpression : MonoBehaviour {
    Expressions.ExpressionType type;
    ExpressionSet expSet;
    List<Transform> expressions;
    Transform range;
    bool initialized = false;

    void Awake () {
        if (initialized) return;
        type = Expressions.ExpressionType.VecField;
        expressions = new List<Transform>();
        initialized = true;
    }

    public void Initialize()
    {
        if (!initialized)
        {
            type = Expressions.ExpressionType.VecField;
            expressions = new List<Transform>();
            initialized = true;
        }
    }

    public ExpressionSet getExpSet()
    {
        return expSet;
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
