using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Reflection;
using System;
using UnityEngine;

public abstract class ReenactableType
{
    protected abstract ReenactableAction[] Actions { get; }
    protected abstract Type TargetType { get; }

    public ReenactableAction[] GetReenactors()
    {
        return Actions;
    }
    public Type GetTargetType()
    {
        return TargetType;
    }

    public virtual void TryAddLogger(GameObject go)
    {
        Type targetType = GetTargetType();
        if (targetType != null && go.GetComponent(targetType) != null)
        {
            AddLogger(go);
        }
    }

    protected abstract void AddLogger(GameObject go);
}
