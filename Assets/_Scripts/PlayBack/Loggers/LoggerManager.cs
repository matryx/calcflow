using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Reflection;
using Extensions;
using CalcFlowUI;
using UnityEngine;

public static class LoggerManager
{
    private static List<ReenactableType> loggerList = new List<ReenactableType> {
        new ButtonReenactableType(),
        new ReenactabeTypeTransform(),
    };

    public static void SetupLoggers(GameObject gobj)
    {
        foreach (ReenactableType t in loggerList)
        {
            t.TryAddLogger(gobj);
        }
    }

    public static void SetupReenactors()
    {
        foreach (ReenactableType t in loggerList)
        {
            foreach (ReenactableAction action in t.GetReenactors())
            {
                PlaybackLogEntry.RegisterReenactor(action.key, action.Reenact);
            }
        }
    }
}