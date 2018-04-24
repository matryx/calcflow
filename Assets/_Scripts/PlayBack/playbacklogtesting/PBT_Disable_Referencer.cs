using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;
using CalcFlowUI;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public class PBT_Disable_Referencer : MonoBehaviour
{


    public Button disabledObject;
    void Start()
    {
        if (disabledObject == null) Debug.Log("Disabled reference lost.");
    }

    // Update is called once per frame
    void Update()
    {

    }
}
