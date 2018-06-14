using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MatrixExpression : MonoBehaviour 
{
    Expressions expressionsClass;
	MatrixSet mtxSet;
	MatrixActions mtxActions;

    List<Transform> matricesList;

    Transform separator;
	
	Scroll scroll;
	bool initialized = false;
    float xPos = 1.2f;
    bool destroyCalled = false;
    bool isActive = true;

    void Awake()
    {
        if (initialized) return;
        expressionsClass = Expressions._instance;
        mtxSet = new MatrixSet();
        matricesList = new List<Transform>();
        mtxActions = transform.GetChild(0).GetChild(0).GetChild(0).GetComponentInChildren<MatrixActions>();

        scroll = expressionsClass.getScroll(Expressions.ExpressionType.PARAMET);
        initialized = true;
    }

    public void Initialize()
    {
        if (!initialized)
        {
            matricesList = new List<Transform>();
            initialized = true;
        }
    }

	public MatrixActions getMtxActions()
    {
        return mtxActions;
    }

    public void setActiveStatus(bool status)
    {
        isActive = status;
    }

    public bool getActiveStatus()
    {
        return isActive;
    }

    public Scroll getScroll()
    {
        return scroll;
    }
    public void setButtonInputColor(Color col)
    {
        foreach (Transform t in matricesList)
        {
            t.Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
        }
    }

    public void setTextColor(Color c)
    {
        foreach (Transform t in matricesList)
        {
            foreach (Transform child in t)
            {
                if (child.GetComponent<TMPro.TextMeshPro>())
                {
                    child.GetComponent<TMPro.TextMeshPro>().color = c;
                }
            }
        }
    }
    public void setSeparator(Transform sep)
    {
        separator = sep;
    }

    public Transform getSeparator()
    {
        return separator;
    }

    public MatrixSet getMtxSet()
    {
        return mtxSet;
    }
    public void addExpression(Transform expr)
    {
        matricesList.Add(expr);
    }

    //TODO: fix the fade in from scroll (fading in too early, when it's still out of the board's dimensions
    //      - make it so that objects don't start fading in until they're inside the board's dimensions
    public void deleteExpressionFromScroll()
    {
        matricesList.Add(separator);
        scroll.deleteObjects(matricesList);
    }
}
