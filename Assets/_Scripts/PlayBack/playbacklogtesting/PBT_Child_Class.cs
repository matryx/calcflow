using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public class PBT_Child_Class : PBT_Abstract_Class
{

    public int concrete_int = 1;
    public string concrete_string = "default";



    // Update is called once per frame
    void Update()
    {

    }
}
