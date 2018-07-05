using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using UnityEngine;
using CalcFlowUI;
using System;
using System.Reflection;




public static class LoggerManager
{

    //ADD YOUR LOGGERS HERE
    public static List<(Type, Type)> loggerList = new List<(Type, Type)> {
        (typeof(ButtonLogger), typeof(Button)),

        typeof(MovementLogger)
        };

    public static void SetupLoggers()
    {
        foreach (Type t in loggerList)
        {
            if (!t.IsSubclassOf(typeof(PlayBackLogger)))
            {
                Debug.LogError("Class " + t + " does not inherit " + typeof(PlayBackLogger));
            }
            MethodInfo m = t.GetMethod("AddLoggers", BindingFlags.Static | BindingFlags.Public | BindingFlags.FlattenHierarchy);
            if (m == null)
            {
                Debug.LogError("Class " + t + " does not have method AddLoggers.");
            }

            m.Invoke(null, null);
        }
    }

}
