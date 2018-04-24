using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public class PBT_Abstract_referencer : MonoBehaviour
{
    public PBT_Abstract_Class abstract_reference;
    public PBT_Child_Class concrete_reference;
}
