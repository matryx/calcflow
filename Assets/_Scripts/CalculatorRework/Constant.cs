using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Constant : MonoBehaviour, ExpressionTabInterface
{
    Expressions.ExpressionType type;
    List<Transform> components;
    Transform separator;
    bool initialized = false;
    bool isActive;

    //NOTE: list not necessary just need to figure out how to group constants together and where to put them

	void Awake () {
        if (initialized) return;
        type = Expressions.ExpressionType.CONSTANT;
        components = new List<Transform>();
        initialized = true;
	}

    public void Initialize()
    {
        if (!initialized)
        {
            type = Expressions.ExpressionType.CONSTANT;
            components = new List<Transform>();
            initialized = true;
        }
    }

    public void setExpressionX(Transform sep)
    {
        separator = sep;
    }

    public Transform getExpressionX()
    {
        return separator;
    }

    public void addComponent(Transform comp)
    {
        components.Add(comp);
    }

    //NOT NEEDED
    public bool getActiveStatus()
    {
        return isActive;
    }

    void Update () {
		
	}
}
