using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Constant : MonoBehaviour {
    Expressions.ExpressionType type;
    List<Transform> components;
    Transform separator;
    bool initialized = false;

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

    public void setSeparator(Transform sep)
    {
        separator = sep;
    }

    public Transform getSeparator()
    {
        return separator;
    }

    public void addComponent(Transform comp)
    {
        components.Add(comp);
    }

	void Update () {
		
	}
}
