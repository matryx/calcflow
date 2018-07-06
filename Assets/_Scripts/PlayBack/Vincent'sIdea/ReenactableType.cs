using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Reflection;
using System;
using UnityEngine;

public abstract class ReenactableType
{
    protected abstract ReenactableAction[] Actions { get; }
    public ReenactableAction[] GetReenactors()
    {
        return Actions;
    }
    public static void AddLoggers(Type targetType)
    {
        List<Component> objectsInScene = new List<Component>();
        Component[] comps = Resources.FindObjectsOfTypeAll(targetType) as Component[];
        foreach (Component co in comps)
        {
            if (co.gameObject.hideFlags == HideFlags.NotEditable || co.gameObject.hideFlags == HideFlags.HideAndDontSave)
                continue;
            if (co.gameObject.scene.name == null)
                continue;
            objectsInScene.Add(co);
        }
    }

    public ReenactableType() { }

}
