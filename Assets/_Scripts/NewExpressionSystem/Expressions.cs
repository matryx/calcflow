using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Expressions : MonoBehaviour
{
    Transform selectedExpression;
    List<Transform> expressions;
    public enum ExpressionType { Constant, Paramet, VecField }
    //public enum Action { Add, Remove, Hide, Flowline }

    public Transform remove, hide, flowLine;
    Color activeColor, inactiveColor;

    //TODO: managing expressions
    // - need to keep track of selected Expr
    // - make it so that selected Expr affects which Actions are shown, the graph and output destination
    void Awake()
    {
        expressions = new List<Transform>();
        remove.gameObject.SetActive(true);
        hide.gameObject.SetActive(true);
        flowLine.gameObject.SetActive(true);

        activeColor = remove.GetComponentInChildren<Renderer>().material.color;
        float gray = activeColor.grayscale;
        inactiveColor = new Color(gray, gray, gray);
    }

    public void addExpr(Transform exp)
    {
        expressions.Add(exp);
        exp.SetParent(transform);
    }

    public void setSelectedExpr(Transform expr)
    {
        selectedExpression = expr;

        //display proper actions
        if (expr.GetComponent<ParametricExpression>())
        {
            hide.GetComponentInChildren<Collider>().enabled = true;
            flowLine.GetComponentInChildren<Collider>().enabled = false;

            hide.GetComponentInChildren<Renderer>().material.color = activeColor;
            flowLine.GetComponentInChildren<Renderer>().material.color = inactiveColor;
        }
        else if (expr.GetComponent<VectorFieldExpression>())
        {
            hide.GetComponentInChildren<Collider>().enabled = false;
            flowLine.GetComponentInChildren<Collider>().enabled = true;

            hide.GetComponentInChildren<Renderer>().material.color = inactiveColor;
            flowLine.GetComponentInChildren<Renderer>().material.color = activeColor;
        }
        else if (expr.GetComponent<Constant>())
        {
            hide.GetComponentInChildren<Collider>().enabled = false;
            flowLine.GetComponentInChildren<Collider>().enabled = false;

            hide.GetComponentInChildren<Renderer>().material.color = inactiveColor;
            flowLine.GetComponentInChildren<Renderer>().material.color = inactiveColor;
        }
    }

    void Update()
    {

    }
}
