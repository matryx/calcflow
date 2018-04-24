using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public abstract class PBT_Abstract_Class : MonoBehaviour
{

    public int abstract_int = 1;

    public string abstract_string = "default";


}
