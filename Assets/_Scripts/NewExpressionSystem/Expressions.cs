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
    // 2 - integrate secondary menu with new calc
    // 3 - get generate mesh and save expression working with new calc system 
    // 4 - add hover text to buttons
    // 5 - do room set up to get rid of million warning messages
    //
    // nice to haves - 
    //  1 - slide variable shortcuts in and out 
    //  2 - enable underscore movement by raycast hit

    //BUGS:
    // 1 - parametric won't graph until you type a variable
    // 2 - no option to have no vec fields showing
    // 3 - fading in isn't quite working (not lerping like fadeOut does)
    // 4 - UI of paramet getting disabled when you switch to vecor tab and back
    // 5 - outputmanager and vecfield manager's update threw null error but couldn't replicate bug

    //DONE:
    // 1 - got presetMenu showing up properly and graphing default but UI not updating unless you select body
    // 2 - major refactoring and fixed bugs caused by refactor

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
