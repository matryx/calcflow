using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Expressions : MonoBehaviour {
    Scroll expressionScroll;

	void Awake () {
        expressionScroll = GetComponentInChildren<Scroll>();

        GameObject selector = Instantiate(Resources.Load("Expressions/ExpressionSelector", typeof(GameObject))) as GameObject;
        GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;

        expressionScroll.addObject(selector.transform);
        expressionScroll.addObject(sep.transform);
    }

    void Update () {
		
	}
}
