using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Reflection;
using CalcFlowUI;
using UnityEngine;

public static class LoggerManager
{
    //ADD YOUR LOGGERS HERE
    // Format:
    // {typeof(<loggerClass>), typeof (<targetClass>)}
    private static TupleList<Type, Type> loggerList = new TupleList<Type, Type> {
        { typeof (ButtonLogger), typeof (Button) },
        { typeof (MovementLogger), typeof (Transform) }
    };

    public class Tuple<T1, T2>
    {

        public T1 First;
        public T2 Second;
        public Tuple(T1 first, T2 second)
        {
            First = first;
            Second = second;
        }
    }
    private class TupleList<T1, T2> : List<Tuple<T1, T2>>
    {
        public void Add(T1 item, T2 item2)
        {
            Add(new Tuple<T1, T2>(item, item2));
        }
    }

    public static void SetupLoggers()
    {
        foreach (Tuple<Type, Type> t in loggerList)
        {
            Debug.Log(t.First);
            if (!t.First.IsSubclassOf(typeof(PlayBackLogger)))
            {
                Debug.LogError("Class " + t.First + " does not inherit " + typeof(PlayBackLogger));
            }
            MethodInfo m = t.First.GetMethod("AddLoggers", BindingFlags.Static | BindingFlags.Public | BindingFlags.FlattenHierarchy);
            if (m == null)
            {
                Debug.LogError("Class " + t.First + " does not have method AddLoggers.");
            }
            object[] parameters = { t.Second };
            m.Invoke(null, parameters: parameters);
        }
    }

    public static void SetupReenactors()
    {
        foreach (Tuple<Type, Type> t in loggerList)
        {
            Debug.Log(t.First);
            if (!t.First.IsSubclassOf(typeof(ReenactableType)))
            {
                Debug.LogError("Class " + t.First + " does not inherit " + typeof(ReenactableType));
            }

            ConstructorInfo c = t.First.GetConstructor(
                BindingFlags.Instance | BindingFlags.Public, null,
                CallingConventions.HasThis, new Type[0], null);

            if (c == null)
            {
                Debug.LogError("Class " + t.First + " does not have an empty constructor.");
            }
            ReenactableType con = c.Invoke(null);
        }
    }

}