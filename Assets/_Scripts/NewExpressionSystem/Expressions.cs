using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Expressions : MonoBehaviour
{
    Transform selectedExpression;
    ExpressionBody selectedBody;
    List<Transform> expressions;
    public enum ExpressionType { Constant, Paramet, VecField }

    public Transform remove, hide, flowLine;
    Color actionActiveColor, actionInactiveColor;
    Color expressionActiveColor, expressionInactiveColor;

    void Awake()
    {
        expressions = new List<Transform>();
        remove.gameObject.SetActive(true);
        hide.gameObject.SetActive(true);
        flowLine.gameObject.SetActive(true);

        actionActiveColor = remove.Find("Body").GetComponent<Renderer>().material.color;
        actionInactiveColor = Color.gray;

        ColorUtility.TryParseHtmlString("#64C3A7FF", out expressionActiveColor);
        ColorUtility.TryParseHtmlString("#FFFFFFFF", out expressionInactiveColor);
    }

    public void addExpr(Transform exp)
    {
        expressions.Add(exp);
        exp.SetParent(transform);
    }

    public Transform getSelectedExpr()
    {
        return selectedExpression;
    }

    public void setSelectedExpr(Transform expr, ExpressionBody body)
    {
        if (selectedBody) selectedBody.unSelect();
        selectedExpression = expr;
        selectedBody = body;

        remove.gameObject.SetActive(true);
        hide.gameObject.SetActive(true);
        flowLine.gameObject.SetActive(true);

        if (expr.GetComponent<ParametricExpression>())
        {
            hide.GetComponentInChildren<Collider>().enabled = true;
            flowLine.GetComponentInChildren<Collider>().enabled = false;

            hide.GetComponentInChildren<Renderer>().material.color = actionActiveColor;
            flowLine.GetComponentInChildren<Renderer>().material.color = actionInactiveColor;
        }
        else if (expr.GetComponent<VectorFieldExpression>())
        {
            hide.GetComponentInChildren<Collider>().enabled = false;
            flowLine.GetComponentInChildren<Collider>().enabled = true;

            hide.GetComponentInChildren<Renderer>().material.color = actionInactiveColor;
            flowLine.GetComponentInChildren<Renderer>().material.color = actionActiveColor;
        }
        else if (expr.GetComponent<Constant>())
        {
            hide.GetComponentInChildren<Collider>().enabled = false;
            flowLine.GetComponentInChildren<Collider>().enabled = false;

            hide.GetComponentInChildren<Renderer>().material.color = actionInactiveColor;
            flowLine.GetComponentInChildren<Renderer>().material.color = actionInactiveColor;
        }
    }

    void Update()
    {

    }
}
