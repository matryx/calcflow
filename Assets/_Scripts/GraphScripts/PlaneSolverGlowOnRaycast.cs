using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

public class PlaneSolverGlowOnRaycast : GlowOnRaycast {
    [SerializeField]
    public List<string> IgnoreNames;
    public override void RecursiveChangeLayer(Transform transform, int toLayer)
    {
            transform.gameObject.layer = toLayer;

        int childCount = transform.childCount;
        for (int i = 0; i < childCount; i++)
        {
            if (!IgnoreNames.Contains(transform.GetChild(i).name))
            {
                RecursiveChangeLayer(transform.GetChild(i), toLayer);
            }
   //         if (transform.GetChild(i).name != "Planecontainer") {
			//	RecursiveChangeLayer(transform.GetChild(i), toLayer);
			//}
        }
    }
}
