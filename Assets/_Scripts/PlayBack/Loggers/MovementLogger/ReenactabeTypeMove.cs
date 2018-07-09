
using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

public class ReenactabeTypeMove : ReenactableType
{
    private ReenactableAction[] reenactors = {
        new ReenactableActionMove(),
    };
    protected override ReenactableAction[] Actions
    {
        get
        {
            return reenactors; ;
        }
    }
    protected override Type TargetType { get { return typeof(Transform); } }

    protected override void AddLogger(GameObject go)
    {
		go.EnsureOneOf<ReenactableLoggerMove>();
    }
}
