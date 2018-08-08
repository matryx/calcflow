using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class Expressions : MonoBehaviour
{
    public static Expressions _instance;
    public enum ExpressionType { CONSTANT, PARAMET, VECFIELD }

    CalculatorManager calcManager;
    Scroll paramScroll, vecFieldScroll, constantScroll;

    ExpressionSet selectedExpSet;
    ExpressionBody selectedBody;
    ExpressionBody selectedVecBody;

    [SerializeField]
    Transform selectedExpression;

    List<Transform> expressions;

    //TODO:
    // 1 -  fix pi symbol
    // 2 - make add button bigger
    //
    // nice to haves - 
    //  1 - slide variable shortcuts in and out 
    //  2 - enable underscore movement by raycast hit

    //BUGS:
    // 1 - parametric won't graph until you type a variable

    //DONE:
    // 1 - fixed fade in when things coming in from bottom but not from top
    // 2 - fixed feedback issue
    // 3 - added error popups for vec field
    // 4 - replaced all texts with tmpro/image

    void Awake()
    {
        _instance = this;
        calcManager = ParametricManager._instance;

        paramScroll = transform.parent.Find("ParametrizationPanel").GetComponentInChildren<Scroll>();
        vecFieldScroll = transform.parent.Find("VectorFieldPanel").GetComponentInChildren<Scroll>();
        constantScroll = transform.parent.Find("ConstantPanel").GetComponentInChildren<Scroll>();

        expressions = new List<Transform>();
    }

    public void setManager(CalculatorManager cm)
    {
        calcManager = cm;
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

    public void reselectVecExpression()
    {
        if (selectedVecBody != null) selectedVecBody.selectBody();
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

        if (calcManager == VecFieldManager._instance)
        {
            selectedVecBody = selectedBody;
        }
    }

    public bool selectedNotNull()
    {
        return (selectedExpression != null);
    }

    public void deleteExpression(Transform del)
    {
        calcManager.RemoveExpressionSet(del.gameObject.GetInterface<ExpressionTabInterface>().getExpSet());
        del.gameObject.GetInterface<ExpressionTabInterface>().deleteExpressionFromScroll();
        expressions.Remove(del);
    }

    void Update() { }
}
