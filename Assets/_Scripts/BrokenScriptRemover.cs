using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;

[ExecuteInEditMode]
public class BrokenScriptRemover
{

    [MenuItem("MyMenu/SelectMissing")]
    static void SelectMissing(MenuCommand command)
    {
        List<GameObject> ts = GetAllObjectsInScene();
        List<GameObject> selection = new List<GameObject>();
        foreach (GameObject t in ts)
        {
            Component[] cs = t.gameObject.GetComponents<Component>();
            foreach (Component c in cs)
            {
                if (c == null)
                {
					Debug.Log("selecting");
                    selection.Add(t.gameObject);
                }
            }
        }
        Selection.objects = selection.ToArray();
    }

    static List<GameObject> GetAllObjectsInScene()
    {
        List<GameObject> objectsInScene = new List<GameObject>();

        foreach (GameObject go in Resources.FindObjectsOfTypeAll(typeof(GameObject)) as GameObject[])
        {
            if (go.hideFlags == HideFlags.NotEditable || go.hideFlags == HideFlags.HideAndDontSave)
                continue;

            if (go.scene.name == null)
                continue;

            objectsInScene.Add(go);
        }

        return objectsInScene;
    }
}
