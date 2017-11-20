using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Expressions : MonoBehaviour
{
    CalculatorManager calcManager;
    public static Expressions _instance;
    ExpressionSet selectedExpSet;
    Transform selectedExpression;
    ExpressionBody selectedBody;
    List<Transform> expressions;
    public enum ExpressionType { Constant, Paramet, VecField }

    public Transform remove, hide, flowLine;
    Color actionActiveColor, actionInactiveColor;
    //Color expressionActiveColor, expressionInactiveColor;

    void Awake()
    {
        _instance = this;
        calcManager = CalculatorManager._instance;
        expressions = new List<Transform>();
        remove.gameObject.SetActive(true);
        hide.gameObject.SetActive(true);
        flowLine.gameObject.SetActive(true);

        actionActiveColor = remove.Find("Body").GetComponent<Renderer>().material.color;
        actionInactiveColor = Color.gray;

        //ColorUtility.TryParseHtmlString("#64C3A7FF", out expressionActiveColor);
        //ColorUtility.TryParseHtmlString("#FFFFFFFF", out expressionInactiveColor);
    }

    public Scroll getParametricScroll()
    {
        return transform.parent.Find("ParametrizationPanel").GetComponentInChildren<Scroll>();
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

    public ExpressionBody getSelectedBody()
    {
        return selectedBody;
    }

    public bool selectedNotNull()
    {
        return (selectedExpression != null);
    }

    public void setSelectedExpr(Transform expr, ExpressionBody body)
    {
        if (expr == null && body == null)
        {
            selectedExpression = expr;
            //print("selected expr: " + selectedExpression);
            selectedBody = null;
            remove.gameObject.SetActive(false);
            hide.gameObject.SetActive(false);
            flowLine.gameObject.SetActive(false);
            return;
        }

        if (selectedBody) selectedBody.unSelect();
        selectedExpression = expr;
        selectedBody = body;

        //print("selected expr: " + selectedExpression);

        remove.gameObject.SetActive(true);
        hide.gameObject.SetActive(true);
        flowLine.gameObject.SetActive(true);

        if (!calcManager) calcManager = CalculatorManager._instance;

        if (expr.GetComponent<ParametricExpression>())
        {
            hide.GetComponentInChildren<Collider>().enabled = true;
            flowLine.GetComponentInChildren<Collider>().enabled = false;

            hide.GetComponentInChildren<Renderer>().material.color = actionActiveColor;
            flowLine.GetComponentInChildren<Renderer>().material.color = actionInactiveColor;

            selectedExpSet = expr.GetComponent<ParametricExpression>().getExpSet();
            calcManager.ChangeExpressionSet(selectedExpSet);
        }
        else if (expr.GetComponent<VectorFieldExpression>())
        {
            hide.GetComponentInChildren<Collider>().enabled = false;
            flowLine.GetComponentInChildren<Collider>().enabled = true;

            hide.GetComponentInChildren<Renderer>().material.color = actionInactiveColor;
            flowLine.GetComponentInChildren<Renderer>().material.color = actionActiveColor;

            selectedExpSet = expr.GetComponent<VectorFieldExpression>().getExpSet();
            calcManager.ChangeExpressionSet(selectedExpSet);
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
