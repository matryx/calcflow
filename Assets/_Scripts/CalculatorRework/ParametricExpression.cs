using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParametricExpression : MonoBehaviour {
    Expressions.ExpressionType type;
    List<Transform> components;

    void Awake () {
        type = Expressions.ExpressionType.Paramet;
    }

    public void addComponent(Transform comp)
    {
        components.Add(comp);
    }

    void Update () {
		
	}
}
