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


	// Use this for initialization
	void Start () 
	{
		
	}
	
	// Update is called once per frame
	void Update () 
	{
		
	}
}
