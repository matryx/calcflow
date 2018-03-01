using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Expressions : MonoBehaviour
{
    CalculatorManager calcManager;
    Scroll paramScroll, vecFieldScroll, constantScroll;
    public static Expressions _instance;
    ExpressionSet selectedExpSet;
    Transform selectedExpression;
    ExpressionBody selectedBody;
    List<Transform> expressions;
    public enum ExpressionType { Constant, Paramet, VecField }

    public Transform remove, hide, flowLine;
    Color actionActiveColor, actionInactiveColor;
    //Color expressionActiveColor, expressionInactiveColor;

    //TODO:
    // 2 - grayed out example message in constants
    // 3 - slide variable shortcuts in and out 
    // 4 - enable underscore movement by raycast hit
    // 5 - hamburger menu for deleting/disabling variables and expressions - IN PROGRESS
    // 6 - figure out how to differentiate Euler's number with regular e letter (italicize?) 


    //VAR DELETE UI IDEA:
    //make an action button that makes expression enter a delete mode
    //if delete mode on:
    //  - two new buttons pop up (one for deleting var one for expression)
    //  - highlights variables, if selected, the highlight for that var turns red
    //if delete var button selected:
    //  - deletes var
    //if delete expression selected:
    //  - pull up delete confirmation, delete if confirmed, otherwise make popup disappear
    //if delete mode off:
    //  - delete buttons disappear

    //  - need to figure out how to differentiate between expr delete and var delete button
    //  - clicking 

    //BUGS:
    // 1 - typing letters in vector fields creating variables in parametrization tab (handle in the future)
    // 2 - creating empty game object everytime a letter is pressed
    
    void Awake()
    {
        _instance = this;
        calcManager = CalculatorManager._instance;
        paramScroll = transform.parent.Find("ParametrizationPanel").GetComponentInChildren<Scroll>();
        vecFieldScroll = transform.parent.Find("VectorFieldPanel").GetComponentInChildren<Scroll>();
        constantScroll = transform.parent.Find("ConstantPanel").GetComponentInChildren<Scroll>();

        expressions = new List<Transform>();
        remove.gameObject.SetActive(true);
        hide.gameObject.SetActive(true);
        flowLine.gameObject.SetActive(true);

        actionActiveColor = remove.Find("Body").GetComponent<Renderer>().material.color;
        actionInactiveColor = Color.gray;
        //ColorUtility.TryParseHtmlString("#64C3A7FF", out expressionActiveColor);
        //ColorUtility.TryParseHtmlString("#FFFFFFFF", out expressionInactiveColor);
    }

    public Scroll getScroll(string type)
    {
        switch (type)
        {
            case "param":
                return paramScroll;
            case "vec":
                return vecFieldScroll;
            case "cons":
                return constantScroll;
        }

        print("GET SCROLL RETURNED NULL");
        return null;
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

    public ExpressionSet getSelectedExprSet()
    {
        return selectedExpSet;
    }

    public bool selectedNotNull()
    {
        return (selectedExpression != null);
    }

    public void deleteExpression()
    {
        expressions.Remove(selectedExpression);

        if (selectedExpression.GetComponent<ParametricExpression>())
        {
            //deletes expression from graph
            //have to do this first because deleting UI sets expression set to null
            calcManager.RemoveExpressionSet(selectedExpSet);
            //deletes expression from UI
            selectedExpression.GetComponent<ParametricExpression>().deleteExpressionFromScroll();
        }
    }

    public void setSelectedExpr(Transform expr, ExpressionBody body)
    {
        if (expr == null)
        {
            if (selectedExpression.GetComponent<ParametricExpression>())
            {
                selectedExpression.GetComponent<ParametricExpression>().disableActions();
            }
            selectedExpression = expr;
            selectedBody = null;
            selectedExpSet = null;
            remove.gameObject.SetActive(false);
            hide.gameObject.SetActive(false);
            flowLine.gameObject.SetActive(false);
            return;
        }

        selectedExpression = expr;
        selectedBody = body;

        if (!calcManager) calcManager = CalculatorManager._instance;

        if (expr.GetComponent<ParametricExpression>())
        {
            selectedExpSet = expr.GetComponent<ParametricExpression>().getExpSet();
            calcManager.ChangeExpressionSet(selectedExpSet);

            expr.GetComponent<ParametricExpression>().enableActions();
        }
        else if (expr.GetComponent<VectorFieldExpression>())
        {
            selectedExpSet = expr.GetComponent<VectorFieldExpression>().getExpSet();
            calcManager.ChangeExpressionSet(selectedExpSet);
        }
        else if (expr.GetComponent<Constant>())
        {
            //TODO: implement constants
        }
    }

    void Update() { }
}
