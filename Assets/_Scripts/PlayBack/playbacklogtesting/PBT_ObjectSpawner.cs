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
        if (Replayer.Replaying) return;

        if (elapsedTime > 2)
        {

            Instantiate(Resources.Load("Prefabs\\PBT_Prefabs\\Sphere", typeof(GameObject)));
            elapsedTime = 0;
        }

        elapsedTime = elapsedTime + Time.deltaTime;
    }
}
