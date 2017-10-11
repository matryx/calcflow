using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Constant : MonoBehaviour {
    Expressions.ExpressionType type;
    List<Transform> components;

    //NOTE: list not necessary just need to figure out how to group constants together and where to put them

	void Awake () {
        type = Expressions.ExpressionType.Constant;
        components = new List<Transform>();
	}
	
    public void addComponent(Transform comp)
    {
        components.Add(comp);
    }

	void Update () {
		
	}
}
