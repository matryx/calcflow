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
    // 1 - fix pi symbol
    // 2 - make add button bigger
    // 3 - edit files to follow coding standard
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

    public void SetManager(CalculatorManager cm)
    {
        calcManager = cm;
    }

    public Scroll GetScroll(ExpressionType type)
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

    public void AddExpr(Transform exp)
    {
        expressions.Add(exp);
        exp.SetParent(transform);
    }

    public Transform GetSelectedExpr()
    {
        return selectedExpression;
    }

    public ExpressionBody GetSelectedBody()
    {
        return selectedBody;
    }

    public ExpressionSet GetSelectedExprSet()
    {
        return selectedExpSet;
    }

    public void ReselectVecExpression()
    {
        if (selectedVecBody != null) selectedVecBody.SelectBody();
    }

    public void SetSelectedExpr(Transform expr, ExpressionBody body)
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

    public bool SelectedNotNull()
    {
        return (selectedExpression != null);
    }

    public void DeleteExpression(Transform del)
    {
        calcManager.RemoveExpressionSet(del.gameObject.GetInterface<ExpressionTabInterface>().GetExpSet());
        del.gameObject.GetInterface<ExpressionTabInterface>().DeleteExpressionFromScroll();
        expressions.Remove(del);
    }

    void Update() { }
}
