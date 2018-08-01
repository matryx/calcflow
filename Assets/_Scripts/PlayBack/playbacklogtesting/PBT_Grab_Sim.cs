using System.Collections;
using System.Collections.Generic;
using Extensions;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public class PBT_Grab_Sim : QuickButton
{

    public GameObject grabTarget;
    private GameObject pivot;
    protected string EmptyUIDPrefab = "Prefabs/EmptyUID";
    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (!Replayer.Replaying)
        {
            StartCoroutine(GrabSim());
        }
    }

    IEnumerator GrabSim()
    {
        pivot = RSUtility.Instantiate(Resources.Load(EmptyUIDPrefab, typeof(GameObject)) as GameObject) as GameObject;
        grabTarget.transform.parent = pivot.transform;
        yield return null;
        pivot.MoveTo(new Vector3(3f, 0, 0), 1);
        yield return new WaitForSeconds(1.1f);
        grabTarget.transform.parent = null;
        Destroy(pivot);
    }

    protected override void ButtonExitBehavior(GameObject other) { }
}