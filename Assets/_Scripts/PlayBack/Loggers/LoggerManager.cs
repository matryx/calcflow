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
        new ReenactabeTypeMove(),
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
                PlaybackLogAction2.RegisterReenactor(action.key, action.Reenact);
            }
        }
    }
}