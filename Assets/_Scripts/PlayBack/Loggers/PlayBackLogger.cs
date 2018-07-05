using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using UnityEngine;
using System;
using System.Reflection;

public abstract class PlayBackLogger : Nanome.Core.Behaviour
{



    public static void AddLoggers(Type t)
    {
        List<MonoBehaviour> objectsInScene = new List<MonoBehaviour>();

        foreach (MonoBehaviour go in Resources.FindObjectsOfTypeAll(t) as MonoBehaviour[])
        {
            if (go.gameObject.hideFlags == HideFlags.NotEditable || go.gameObject.hideFlags == HideFlags.HideAndDontSave)
                continue;
            if (go.gameObject.scene.name == null)
                continue;

            objectsInScene.Add(go);
        }
    }
}
