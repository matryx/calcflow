using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false, false)]
public class PBT_StaticVars : MonoBehaviour
{
    [RuntimeSerializeField]
    private static string staticVar1 = "staticDefaultVal";

    public string readWrapperForStatic = "";
    public string writeWrapperForStatic = "";
    public bool setStaticVar = false;
    void Update()
    {
        if (setStaticVar)
        {
            staticVar1 = writeWrapperForStatic;
        }
        readWrapperForStatic = staticVar1;
    }
}
