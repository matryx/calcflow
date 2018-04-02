using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false, false)]
public class SerializationTestFF : MonoBehaviour
{
    [RuntimeSerializeField]
    public int RT_testVal = 12;
    [NonRuntimeSerializedField]
    public int nonRT_testVal = 12;
    public bool changeTestVals;

    public void Update()
    {
        if (changeTestVals)
        {
            RT_testVal = 100;
            nonRT_testVal = 100;
            changeTestVals = false;
        }
    }
}
