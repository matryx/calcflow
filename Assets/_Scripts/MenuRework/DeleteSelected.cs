using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]
public class DeleteSelected : QuickButton {
    private List<Transform> expressions;

	protected override void Start ()
    {
        base.Start();
        //FlexButtonComponent[] temp =
        //    transform.parent.GetComponentInChildren<SelectorFlexPanel>().GetComponentsInChildren<FlexButtonComponent>();

        //foreach (FlexButtonComponent f in temp)
        //{
        //    expressions.Add(f.transform);
        //}
	}

    protected override void ButtonEnterBehavior(GameObject other)
    {

    }

    protected override void ButtonExitBehavior(GameObject other)
    {

    }

    void Update () {
		
	}
}
