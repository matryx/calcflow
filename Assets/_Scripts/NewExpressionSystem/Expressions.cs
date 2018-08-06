using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class Expressions : MonoBehaviour
{
    CalculatorManager calcManager;

    Scroll paramScroll, vecFieldScroll, constantScroll;
    public static Expressions _instance;
    ExpressionSet selectedExpSet;
    [SerializeField]
    Transform selectedExpression;
    ExpressionBody selectedBody;
    List<Transform> expressions;
    public enum ExpressionType { CONSTANT, PARAMET, VECFIELD }

    //TODO:
    // 1 - add flowline object
    //
    // nice to haves - 
    //  1 - slide variable shortcuts in and out 
    //  2 - enable underscore movement by raycast hit

    //BUGS:
    // 1 - creating empty game object everytime a letter is pressed
    // 2 - scroll bug? when adding expressions and not at top of page, UI gets broken
    //   - NOTE: pretty sure I already fixed this but need a headset to test
    //           but there still should be a bug: check what happens when X isn't visible 
    //           on scroll and add a new expr while that expr with the hidden x is selected
    //
    //  

    //DONE:
    // 1 - fixed delete bug with vec fields
    // 2 - made it so that action buttons disappear when you cancel a deletion of an expression

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
