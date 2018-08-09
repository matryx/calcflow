using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VectorFieldExpression : MonoBehaviour, ExpressionTabInterface
{
    Expressions expressionsClass;
    ExpressionSet expSet;
    ExpressionActions expActions;

    Scroll scroll;

    Transform range;
    Transform expressionX;
    Transform separator;

    List<Transform> expressionsList;

    bool initialized = false;
    bool isActive = true;

    void Awake()
    {
        if (initialized) return;
        expressionsClass = Expressions._instance;
        expSet = new ExpressionSet();
        expressionsList = new List<Transform>();

        expActions = transform.GetChild(0).GetChild(0).GetChild(0).GetComponentInChildren<ExpressionActions>();
        scroll = expressionsClass.GetScroll(Expressions.ExpressionType.VECFIELD);

        initialized = true;
    }

    public void Initialize()
    {
        if (!initialized)
        {
            expressionsClass = Expressions._instance;
            expSet = new ExpressionSet();
            expressionsList = new List<Transform>();

            expActions = transform.GetChild(0).GetChild(0).GetChild(0).GetComponentInChildren<ExpressionActions>();
            scroll = expressionsClass.GetScroll(Expressions.ExpressionType.VECFIELD);

            initialized = true;
        }
    }

    public void SetRange(Transform r)
    {
        range = r;
    }

    public void SetExpressionX(Transform e)
    {
        expressionX = e;
    }

    public Transform GetExpressionX()
    {
        return expressionX;
    }

    public void SetSeparator(Transform sep)
    {
        separator = sep;
    }

    public Transform GetSeparator()
    {
        return separator;
    }

    public ExpressionSet GetExpSet()
    {
        return expSet;
    }

    public ExpressionActions GetExpActions()
    {
        return expActions;
    }

    public bool GetActiveStatus()
    {
        return isActive;
    }

    public void SetButtonInputColor(Color col)
    {
        foreach (Transform t in expressionsList)
        {
            t.Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
        }

        range.Find("Min").Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
        range.Find("Max").Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
    }

    public void SetElementQuadTex(Texture tex)
    {
        range.GetChild(0).Find("Quad").GetComponent<Renderer>().material.mainTexture = tex;
    }

    public void SetTextColor(Color c)
    {
        foreach (Transform t in expressionsList)
        {
            foreach (Transform child in t)
            {
                if (child.GetComponent<TMPro.TextMeshPro>())
                {
                    child.GetComponent<TMPro.TextMeshPro>().color = c;
                }
            }
        }

        foreach (Transform child in range)
        {
            foreach (Transform gchild in child)
            {
                if (gchild.GetComponent<TMPro.TextMeshPro>())
                {
                    gchild.GetComponent<TMPro.TextMeshPro>().color = c;
                }
            }
        }
    }

    public void AddExpression(Transform expr)
    {
        expressionsList.Add(expr);
    }

    public void DeleteExpressionFromScroll()
    {
        List<Transform> rest = new List<Transform>();
        rest.Add(range);
        rest.Add(separator);

        scroll.DeleteObjects(expressionsList);
        scroll.DeleteObjects(rest);
    }
    
    void Update() { }
}
