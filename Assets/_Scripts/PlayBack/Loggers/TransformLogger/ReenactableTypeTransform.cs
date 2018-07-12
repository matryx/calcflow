
using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ReenactabeTypeTransform : ReenactableType
{
    private ReenactableAction[] reenactors = {
        new ReenactableActionMove(),
        new ReenactableActionDestroy(),
        new ReenactableActionDisable(),
        new ReenactableActionEnable(),
    };
    protected override ReenactableAction[] Actions
    {
        get
        {
            return reenactors;
        }
    }
    protected override Type TargetType { get { return typeof(Transform); } }

    protected override void AddLogger(GameObject go)
    {
		go.EnsureOneOf<ReenactableLoggerTransform>();
    }
}
