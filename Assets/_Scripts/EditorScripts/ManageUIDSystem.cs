using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;
using Extensions;
[ExecuteInEditMode]


public class ManageUIDSystem : MonoBehaviour
{
    public bool turnOffRecursive = false;
    public bool turnOnRecursive = false;
    public bool addUIDSystem = false;
    public bool removeUIDSysten = false;

    // Use this for initialization
    void Start()
    {

    }

    // Update is called once per frame
    void Update()
    {
        if (turnOffRecursive || turnOnRecursive)
        {
            UIDSystem[] allObjects = UnityEngine.Object.FindObjectsOfType<UIDSystem>();
            foreach (UIDSystem UIDS in allObjects)
            {
                UIDS.SerializeChildren = turnOnRecursive;
            }
            turnOffRecursive = false;
            turnOnRecursive = false;
        }

        if (addUIDSystem)
        {
            GameObject[] allObjects = Resources.FindObjectsOfTypeAll<GameObject>();
            foreach (GameObject obj in allObjects)
            {
                if (obj.GetComponentInParent<AvatarSelector>() == null)
                {
                    obj.EnsureOneOf<UIDSystem>();
                }
            }
            addUIDSystem = false;
        }

        if (removeUIDSysten)
        {
            GameObject[] allObjects = Resources.FindObjectsOfTypeAll<GameObject>();
            foreach (GameObject obj in allObjects)
            {
                obj.EnsureNoneOf<UIDSystem>();
            }
            removeUIDSysten = false;
        }
    }
}
