using System.Collections;
using System.Collections.Generic;
using UnityEngine;


public class PlaneSolverGlowOnRaycast : GlowOnRaycast {


    public override void RecursiveChangeLayer(Transform transform, int toLayer)
    {
            transform.gameObject.layer = toLayer;

        int childCount = transform.childCount;
        for (int i = 0; i < childCount; i++)
        {
            if (transform.GetChild(i).name != "Planecontainer") {
				RecursiveChangeLayer(transform.GetChild(i), toLayer);
			}
        }
    }
}
