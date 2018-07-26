using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VectorFieldExpression : MonoBehaviour, ExpressionTabInterface
{
    Expressions expressionsClass;
    ExpressionSet expSet;
    ExpressionActions expActions;

    List<Transform> expressionsList;

    Transform range;
    Transform expressionX;
    Transform separator;

    Scroll scroll;

    bool initialized = false;
    bool isActive = true;

    void Awake()
    {
        if (initialized) return;
        expressionsClass = Expressions._instance;
        expSet = new ExpressionSet();
        expressionsList = new List<Transform>();

        expActions = transform.GetChild(0).GetChild(0).GetChild(0).GetComponentInChildren<ExpressionActions>();
        scroll = expressionsClass.getScroll(Expressions.ExpressionType.VECFIELD);

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
            scroll = expressionsClass.getScroll(Expressions.ExpressionType.VECFIELD);

            initialized = true;
        }
    }

    public void setRange(Transform r)
    {
        range = r;
    }

    public void setSeparator(Transform sep)
    {
        separator = sep;
    }

    public void setExpressionX(Transform e)
    {
        expressionX = e;
    }

    public Transform getExpressionX()
    {
        return expressionX;
    }

    public ExpressionSet getExpSet()
    {
        return expSet;
    }

    public ExpressionActions getExpActions()
    {
        return expActions;
    }

    public bool getActiveStatus()
    {
        return isActive;
    }

    public void setButtonInputColor(Color col)
    {
        foreach (Transform t in expressionsList)
        {
            t.Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
        }

        range.Find("Min").Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
        range.Find("Max").Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
    }

    public void setElementQuadTex(Texture tex)
    {
        range.GetChild(0).Find("Quad").GetComponent<Renderer>().material.mainTexture = tex;
    }

    public void setTextColor(Color c)
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

    public Scroll getScroll()
    {
        return scroll;
    }

    public void addExpression(Transform expr)
    {
        expressionsList.Add(expr);
    }

    public void deleteExpressionFromScroll()
    {
        List<Transform> rest = new List<Transform>();
        rest.Add(range);
        rest.Add(separator);

        scroll.deleteObjects(expressionsList);
        scroll.deleteObjects(rest);
    }
    
    void Update() { }
}
