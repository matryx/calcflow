using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VectorFieldExpression : MonoBehaviour {
    Expressions.ExpressionType type;
    List<Transform> components;

    void Awake () {
        type = Expressions.ExpressionType.VecField;
    }

    public void addComponent(Transform comp)
    {
        components.Add(comp);
    }

    void Update () {
		
	}
}
