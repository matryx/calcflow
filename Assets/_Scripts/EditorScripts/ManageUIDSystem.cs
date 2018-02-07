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
    public bool removeUIDSystem = false;
    public bool resetUIDSystem = false;
    public bool updateUIDs = false;
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

        if (removeUIDSystem)
        {
            GameObject[] allObjects = Resources.FindObjectsOfTypeAll<GameObject>();
            foreach (GameObject obj in allObjects)
            {
                obj.EnsureNoneOf<UIDSystem>();
            }
            removeUIDSystem = false;
        }

        if (resetUIDSystem)
        {
            UIDSystem[] allObjects = Resources.FindObjectsOfTypeAll<UIDSystem>();
            resetUIDSystem = false;
            foreach (UIDSystem UIDS in allObjects)
            {
                GameObject obj = UIDS.gameObject;
                obj.EnsureNoneOf<UIDSystem>();
                obj.EnsureOneOf<UIDSystem>().UpdateUIDs(false);
            }
            print("success! UIDs fully reset");
        }

        if (updateUIDs)
        {
            UIDSystem[] allObjects = Resources.FindObjectsOfTypeAll<UIDSystem>();
            foreach (UIDSystem UID in allObjects)
            {
                UID.UpdateUIDs(false);
            }
            updateUIDs = false;
        }
    }
}
