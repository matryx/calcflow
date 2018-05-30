using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Expressions : MonoBehaviour
{
    ParametricManager calcManager;
    Scroll paramScroll, vecFieldScroll, constantScroll;
    public static Expressions _instance;
    ExpressionSet selectedExpSet;
    Transform selectedExpression;
    ExpressionBody selectedBody;
    List<Transform> expressions;
    public enum ExpressionType { CONSTANT, PARAMET, VECFIELD }

    //TODO:
    // main functionalities for parametric - DONE / NEED TO TEST HEAVILY
    // nice to haves - 
    //  1 - slide variable shortcuts in and out 
    //  2 - enable underscore movement by raycast hit

    //BUGS:
    // 1 - typing letters in vector fields creating variables in parametrization tab (handle in the future)
    // 2 - creating empty game object everytime a letter is pressed
    // 3 - scroll bug? when adding expressions and not at top of page, UI gets broken
    //      - x input's local position is off

    void Awake()
    {
        _instance = this;
        calcManager = ParametricManager._instance;
        paramScroll = transform.parent.Find("ParametrizationPanel").GetComponentInChildren<Scroll>();
        vecFieldScroll = transform.parent.Find("VectorFieldPanel").GetComponentInChildren<Scroll>();
        constantScroll = transform.parent.Find("ConstantPanel").GetComponentInChildren<Scroll>();

        expressions = new List<Transform>();
    }

    public Scroll getScroll(ExpressionType type)
    {
        switch (type)
        {
            case ExpressionType.PARAMET:
                return paramScroll;
            case ExpressionType.VECFIELD:
                return vecFieldScroll;
            case ExpressionType.CONSTANT:
                return constantScroll;
        }

        print("<color=red>GET SCROLL RETURNED NULL</color>");
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

    public void deleteExpression(Transform del)
    {
        if (del)
        {
            ParametricExpression param = del.GetComponent<ParametricExpression>();
            if (param)
            {
                calcManager.RemoveExpressionSet(param.getExpSet());
                param.deleteExpressionFromScroll();
                expressions.Remove(del);
            }
        }
        else
        {
            if (selectedExpression.GetComponent<ParametricExpression>())
            {
                //deletes expression from graph
                //have to do this first because deleting UI sets expression set to null
                calcManager.RemoveExpressionSet(selectedExpSet);
                //deletes expression from UI
                selectedExpression.GetComponent<ParametricExpression>().deleteExpressionFromScroll();
                expressions.Remove(selectedExpression);
            }
        }
    }

    public void setSelectedExpr(Transform expr, ExpressionBody body)
    {
        if (expr == null)
        {
            selectedExpression = null;
            selectedBody = null;
            selectedExpSet = null;
            return;
        }

        selectedExpression = expr;
        selectedBody = body;

        if (!calcManager) calcManager = ParametricManager._instance;

        //if (expr.GetComponent<ParametricExpression>())
        //{
        //    selectedExpSet = expr.GetComponent<ParametricExpression>().getExpSet();
        //    calcManager.ChangeExpressionSet(selectedExpSet);

        //}
        //else if (expr.GetComponent<VectorFieldExpression>())
        //{
        //    selectedExpSet = expr.GetComponent<VectorFieldExpression>().getExpSet();
        //    calcManager.ChangeExpressionSet(selectedExpSet);
        //}
        //else if (expr.GetComponent<Constant>())
        //{
        //    //TODO: implement constants
        //}
    }

    void Update() { }
}
