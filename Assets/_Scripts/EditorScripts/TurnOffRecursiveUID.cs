using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;
[ExecuteInEditMode]


public class TurnOffRecursiveUID : MonoBehaviour {
    public bool execute = false;
    public bool turnOnInstead = false;

    // Use this for initialization
    void Start()
    {

    }

    // Update is called once per frame
    void Update()
    {
        if (!execute) return;


        UIDSystem[] allObjects = UnityEngine.Object.FindObjectsOfType<UIDSystem>();
        foreach (UIDSystem UIDS in allObjects)
        {
            UIDS.SerializeChildren = turnOnInstead;
        }
        execute = false;
    }
}
