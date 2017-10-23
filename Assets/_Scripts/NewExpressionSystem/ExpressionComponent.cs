using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionComponent : MonoBehaviour {
    Transform expressionParent;

	void Start () {
		
	}

    public void setExpressionParent(Transform p)
    {
        expressionParent = p;
    }

    void Update() { }
}
