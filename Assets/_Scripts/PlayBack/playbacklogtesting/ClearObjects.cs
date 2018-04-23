using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

public class ClearObjects : MonoBehaviour
{

    public bool clearAll = false;

    // Use this for initialization
    void Start()
    {

    }

    void Update()
    {
        if (clearAll)
        {
            ClearAllObjects();
            clearAll = false;
        }
    }
    // Update is called once per frame
    void ClearAllObjects()
    {
        List<UIDSystem> allObjects = GetAllUIDSInScene();
        foreach (UIDSystem u in allObjects)
        {
            if (u.gameObject != gameObject)
                Destroy(u.gameObject);
        }
    }


    List<UIDSystem> GetAllUIDSInScene()
    {
        List<UIDSystem> objectsInScene = new List<UIDSystem>();

        foreach (UIDSystem go in Resources.FindObjectsOfTypeAll(typeof(UIDSystem)) as UIDSystem[])
        {
            if (go.gameObject.hideFlags == HideFlags.NotEditable || go.gameObject.hideFlags == HideFlags.HideAndDontSave)
                continue;
            if (go.gameObject.scene.name == null)
                continue;

            objectsInScene.Add(go);
        }

        return objectsInScene;
    }
}
