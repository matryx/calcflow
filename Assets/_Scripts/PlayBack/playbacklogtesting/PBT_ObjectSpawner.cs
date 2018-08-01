using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false, false)]
public class PBT_ObjectSpawner : MonoBehaviour
{
    private float elapsedTime = 0;
    void Update()
    {
        if (Replayer.Replaying)
        {
            Destroy(this);
            return;
        }

        if (elapsedTime > 2)
        {

            RSUtility.Instantiate(Resources.Load("Prefabs\\PBT_Prefabs\\Sphere", typeof(GameObject)) as GameObject, transform.position, Quaternion.identity);
            elapsedTime = 0;
        }

        elapsedTime = elapsedTime + Time.deltaTime;
    }
}
