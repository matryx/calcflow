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
    // 1 - slide variable shortcuts in and out 
    // 2 - enable underscore movement by raycast hit
    // 3 - delete variable implementation
    // 4 - hide should change the text color of entire expression components to gray    
    //      - should deselect expression
    //      - gray out feedback too                         IMPLEMENTED/NEED TO TEST
    //      - gray out element symbol quad (material)       MOSTLY DONE, JUST NEED TO PROPERLY GREY ELEMENT TEXTURE
    //      - hide action buttons                           IMPLEMENTED/NEED TO TEST
    //      - gray out action button                        IMPLEMENTED/NEED TO TEST

    //VAR DELETE UI IDEA:
    //talk to Kyle
    //for now just make it so that if user deletes variable from all expression components, hide the variable in UI 
    //  -   but save and hide it instead of deleting it so that if user types it again it pops up with same values

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
            //if (selectedExpression.GetComponent<ParametricExpression>())
            //{
            //    //selectedExpression.GetComponent<ParametricExpression>().disableActions();
            //}
            selectedExpression = expr;
            selectedBody = null;
            selectedExpSet = null;
            return;
        }

        selectedExpression = expr;
        selectedBody = body;

        if (!calcManager) calcManager = CalculatorManager._instance;

        if (expr.GetComponent<ParametricExpression>())
        {
            selectedExpSet = expr.GetComponent<ParametricExpression>().getExpSet();
            calcManager.ChangeExpressionSet(selectedExpSet);

            //expr.GetComponent<ParametricExpression>().enableActions();
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
