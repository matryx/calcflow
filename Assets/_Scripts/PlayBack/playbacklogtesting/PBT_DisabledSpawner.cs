using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false, false)]
public class PBT_DisabledSpawner : MonoBehaviour
{
    public GameObject lastSpawned;
    private float elapsedTime = 0;
    void Update()
    {
        if (Replayer.Replaying) return;

        if (elapsedTime > 2)
        {
            if (lastSpawned != null)
            {
                lastSpawned.SetActive(true);
            }
            lastSpawned = RSUtility.Instantiate(Resources.Load("Prefabs\\PBT_Prefabs\\DisabledSphere", typeof(GameObject)) as GameObject, transform.position, Quaternion.identity);

            elapsedTime = 0;
        }

        elapsedTime = elapsedTime + Time.deltaTime;
    }
}
