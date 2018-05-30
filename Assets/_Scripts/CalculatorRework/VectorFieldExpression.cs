using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VectorFieldExpression : MonoBehaviour
{
    Expressions expressionsClass;
    ExpressionSet expSet;
    ExpressionActions expActions;

    List<Transform> expressionsList;

    Transform range;
    Transform separator;
    Scroll scroll;

    bool initialized = false;
    bool isActive = true;

    void Awake () {
        if (initialized) return;
        expressionsClass = Expressions._instance;
        expSet = new ExpressionSet();
        expressionsList = new List<Transform>();

        expActions = transform.GetChild(0).GetChild(0).GetChild(0).GetComponentInChildren<ExpressionActions>();
        scroll = expressionsClass.getScroll(Expressions.ExpressionType.VECFIELD);

        initialized = true;
    }

    public void Initialize()
    {
        if (!initialized)
        {
            expressionsList = new List<Transform>();
            initialized = true;
        }
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

    public bool getActiveStatus()
    {
        return isActive;
    }

    public Scroll getScroll()
    {
        return scroll;
    }

    public void addExpression(Transform expr)
    {
        expressionsList.Add(expr);
    }

    public void setRange(Transform r)
    {
        range = r;
    }

    void Update () {
		
	}
}
