using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;

using Nanome.Core.Extension;

namespace Nanome.Core
{

    public class Delegates
    {

        public delegate void Callback();
        public delegate void Callback<T1>(T1 a1);
        public delegate void Callback<T1, T2>(T1 a1, T2 a2);
        public delegate void Callback<T1, T2, T3>(T1 a1, T2 a2, T3 a3);
        public delegate void Callback<T1, T2, T3, T4>(T1 a1, T2 a2, T3 a3, T4 a4);

        public delegate R Function<R>();
        public delegate R Function<R, T>(T a);
        public delegate R Function<R, T1, T2>(T1 a, T2 b);
        public delegate R Function<R, T1, T2, T3>(T1 a, T2 b, T3 c);
        public delegate R Function<R, T1, T2, T3, T4>(T1 a, T2 b, T3 c, T4 d);

        public static readonly bool runningModificationDebug = Nanome.Core.Config.getBool("runtime-delegates-running-debug", "false");

    }

    public class DelegateList
    {

        List<Delegates.Callback> callbacks;

        bool running = false;

        public DelegateList(int supposedSize = 1)
        {
            callbacks = new List<Delegates.Callback>(supposedSize);
        }

        public void Add(Delegates.Callback callback)
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Add");
                    return;
                }
            }
            callbacks.Add(callback);
        }

        public void Remove(Delegates.Callback callback)
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Remove");
                    return;
                }
            }
            callbacks.Remove(callback);
        }

        public void Clear()
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Clear");
                    return;
                }
            }
            callbacks.Clear();
        }

        public void Invoke()
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Invoke");
                    return;
                }
            }
            running = true;
            var count = callbacks.Count;
            for (var i = 0; i < count; i++)
            {
                try
                {
                    callbacks[i]();
                }
                catch (Exception e)
                {
                    Logs.errorOnChannel("Nanome.Core", "Delegate error", e);
                }
            }
            running = false;
        }

        public void InvokeSafe()
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Invoke");
                    return;
                }
            }
            running = true;
            var count = callbacks.Count;
            for (var i = 0; i < count; i++)
            {
                try
                {
                    callbacks[i]();
                }
                catch (Exception e)
                {
                    Logs.errorOnChannel("Nanome.Core", "DelegateList", "Invoke error", e);
                }
            }
            running = false;
        }

        public int GetCount()
        {
            return callbacks.Count;
        }

    }

    public class DelegateList<ParamType>
    {

        List<Delegates.Callback<ParamType>> callbacks;

        bool running = false;

        public DelegateList(int supposedSize = 1)
        {
            callbacks = new List<Delegates.Callback<ParamType>>(supposedSize);
        }

        public void Add(Delegates.Callback<ParamType> callback)
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Add");
                    return;
                }
            }
            callbacks.Add(callback);
        }

        public void Remove(Delegates.Callback<ParamType> callback)
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Remove");
                    return;
                }
            }
            callbacks.Remove(callback);
        }

        public void Clear()
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Clear");
                    return;
                }
            }
            callbacks.Clear();
        }

        public void Invoke(ParamType param)
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Invoke");
                    return;
                }
            }
            running = true;
            var count = callbacks.Count;
            for (var i = 0; i < count; i++)
            {
                callbacks[i](param);
            }
            running = false;
        }

        public void InvokeSafe(ParamType param)
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Invoke");
                    return;
                }
            }
            running = true;
            var count = callbacks.Count;
            for (var i = 0; i < count; i++)
            {
                try
                {
                    callbacks[i](param);
                }
                catch (Exception e)
                {
                    Logs.errorOnChannel("Nanome.Core", "DelegateList", "Invoke error", e);
                }
            }
            running = false;
        }

        public int GetCount()
        {
            return callbacks.Count;
        }

    }

    public class DelegateList<ParamType1, ParamType2>
    {

        List<Delegates.Callback<ParamType1, ParamType2>> callbacks;

        bool running = false;

        public DelegateList(int supposedSize = 1)
        {
            callbacks = new List<Delegates.Callback<ParamType1, ParamType2>>(supposedSize);
        }

        public void Add(Delegates.Callback<ParamType1, ParamType2> callback)
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Add");
                    return;
                }
            }
            callbacks.Add(callback);
        }

        public void Remove(Delegates.Callback<ParamType1, ParamType2> callback)
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Remove");
                    return;
                }
            }
            callbacks.Remove(callback);
        }

        public void Clear()
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Clear");
                    return;
                }
            }
            callbacks.Clear();
        }

        public void Invoke(ParamType1 param1, ParamType2 param2)
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Invoke");
                    return;
                }
            }
            running = true;
            var count = callbacks.Count;
            for (var i = 0; i < count; i++)
            {
                callbacks[i](param1, param2);
            }
            running = false;
        }

        public void InvokeSafe(ParamType1 param1, ParamType2 param2)
        {
            if (Delegates.runningModificationDebug)
            {
                if (running)
                {
                    Logs.errorOnChannel("Nanome.Core", "Changing delegate list while being invoked", "Invoke");
                    return;
                }
            }
            running = true;
            var count = callbacks.Count;
            for (var i = 0; i < count; i++)
            {
                try
                {
                    callbacks[i](param1, param2);
                }
                catch (Exception e)
                {
                    Logs.errorOnChannel("Nanome.Core", "DelegateList", "Invoke error", e);
                }
            }
            running = false;
        }

        public int GetCount()
        {
            return callbacks.Count;
        }

    }

    public class DelegateDict<KeyType>
    {

        Dictionary<KeyType, DelegateList> callbacksByKey;

        public DelegateDict(int supposedSize = 1)
        {
            callbacksByKey = new Dictionary<KeyType, DelegateList>(supposedSize);
        }

        public void AddAt(KeyType id, Delegates.Callback callback)
        {
            var delegateList = (DelegateList)null;
            if (!callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList = new DelegateList();
                callbacksByKey[id] = delegateList;
            }
            delegateList.Add(callback);
        }

        public void RemoveAt(KeyType id, Delegates.Callback callback)
        {
            var delegateList = (DelegateList)null;
            if (callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList.Remove(callback);
            }
        }

        public void ClearAt(KeyType id)
        {
            var delegateList = (DelegateList)null;
            if (callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList.Clear();
                callbacksByKey.Remove(id);
            }
        }

        public void Clear()
        {
            callbacksByKey.Clear();
        }

        public void InvokeAt(KeyType id)
        {
            var delegateList = (DelegateList)null;
            if (callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList.Invoke();
            }
        }

        public void InvokeSafeAt(KeyType id)
        {
            var delegateList = (DelegateList)null;
            if (callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList.InvokeSafe();
            }
        }

        public int GetCountAt(KeyType id)
        {
            return callbacksByKey[id].GetCount();
        }

        public int GetFullCount()
        {
            int count = 0;
            foreach (var key in callbacksByKey.Keys)
            {
                count += GetCountAt(key);
            }
            return count;
        }

    }

    public class DelegateDict<KeyType, ParamType>
    {

        Dictionary<KeyType, DelegateList<ParamType>> callbacksByKey;

        public DelegateDict(int supposedSize = 1)
        {
            callbacksByKey = new Dictionary<KeyType, DelegateList<ParamType>>(supposedSize);
        }

        public void AddAt(KeyType id, Delegates.Callback<ParamType> callback)
        {
            var delegateList = (DelegateList<ParamType>)null;
            if (!callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList = new DelegateList<ParamType>();
                callbacksByKey[id] = delegateList;
            }
            delegateList.Add(callback);
        }

        public void RemoveAt(KeyType id, Delegates.Callback<ParamType> callback)
        {
            var delegateList = (DelegateList<ParamType>)null;
            if (callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList.Remove(callback);
            }
        }

        public void ClearAt(KeyType id)
        {
            var delegateList = (DelegateList<ParamType>)null;
            if (callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList.Clear();
                callbacksByKey.Remove(id);
            }
        }

        public void Clear()
        {
            callbacksByKey.Clear();
        }

        public void InvokeAt(KeyType id, ParamType param)
        {
            var delegateList = (DelegateList<ParamType>)null;
            if (callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList.Invoke(param);
            }
        }

        public void InvokeSafeAt(KeyType id, ParamType param)
        {
            var delegateList = (DelegateList<ParamType>)null;
            if (callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList.InvokeSafe(param);
            }
        }

        public int GetCountAt(KeyType id)
        {
            return callbacksByKey[id].GetCount();
        }

        public int GetFullCount()
        {
            int count = 0;
            foreach (var key in callbacksByKey.Keys)
            {
                count += GetCountAt(key);
            }
            return count;
        }

    }

    public class DelegateDict<KeyType, ParamType1, ParamType2>
    {

        Dictionary<KeyType, DelegateList<ParamType1, ParamType2>> callbacksByKey;

        public DelegateDict(int supposedSize = 1)
        {
            callbacksByKey = new Dictionary<KeyType, DelegateList<ParamType1, ParamType2>>(supposedSize);
        }

        public void AddAt(KeyType id, Delegates.Callback<ParamType1, ParamType2> callback)
        {
            var delegateList = (DelegateList<ParamType1, ParamType2>)null;
            if (!callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList = new DelegateList<ParamType1, ParamType2>();
                callbacksByKey[id] = delegateList;
            }
            delegateList.Add(callback);
        }

        public void RemoveAt(KeyType id, Delegates.Callback<ParamType1, ParamType2> callback)
        {
            var delegateList = (DelegateList<ParamType1, ParamType2>)null;
            if (callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList.Remove(callback);
            }
        }

        public void ClearAt(KeyType id)
        {
            var delegateList = (DelegateList<ParamType1, ParamType2>)null;
            if (callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList.Clear();
                callbacksByKey.Remove(id);
            }
        }

        public void Clear()
        {
            callbacksByKey.Clear();
        }

        public void InvokeAt(KeyType id, ParamType1 param1, ParamType2 param2)
        {
            var delegateList = (DelegateList<ParamType1, ParamType2>)null;
            if (callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList.Invoke(param1, param2);
            }
        }

        public void InvokeSafeAt(KeyType id, ParamType1 param1, ParamType2 param2)
        {
            var delegateList = (DelegateList<ParamType1, ParamType2>)null;
            if (callbacksByKey.TryGetValue(id, out delegateList))
            {
                delegateList.InvokeSafe(param1, param2);
            }
        }

        public int GetCountAt(KeyType id)
        {
            return callbacksByKey[id].GetCount();
        }

        public int GetFullCount()
        {
            int count = 0;
            foreach (var key in callbacksByKey.Keys)
            {
                count += GetCountAt(key);
            }
            return count;
        }

    }

    public class DelegateMap<KeyType, ParamType>
    {

        Dictionary<KeyType, Delegates.Callback<ParamType>> callbackByKey;

        public DelegateMap(int supposedSize = 0)
        {
            callbackByKey = new Dictionary<KeyType, Delegates.Callback<ParamType>>(supposedSize);
        }

        public bool Set(KeyType id, Delegates.Callback<ParamType> callback)
        {
            var hasKey = callbackByKey.ContainsKey(id);
            if (callback != null)
            {
                callbackByKey[id] = callback;
            }
            else
            {
                callbackByKey.Remove(id);
            }
            return hasKey;
        }

        public bool Invoke(KeyType id, ParamType param)
        {
            var callback = (Delegates.Callback<ParamType>)null;
            if (callbackByKey.TryGetValue(id, out callback))
            {
                callback(param);
                return true;
            }
            return false;
        }

        public bool InvokeSafe(KeyType id, ParamType param)
        {
            try
            {
                return Invoke(id, param);
            }
            catch (Exception e)
            {
                Logs.errorOnChannel("Nanome.Core", "DelegateMap", "Invoke error", e);
            }
            return false;
        }

        public void Clear()
        {
            callbackByKey.Clear();
        }

        public int GetCount()
        {
            return callbackByKey.Count;
        }

    }

    public class DelegateMap<KeyType, ParamType1, ParamType2>
    {

        Dictionary<KeyType, Delegates.Callback<ParamType1, ParamType2>> callbackByKey;

        public DelegateMap(int supposedSize = 0)
        {
            callbackByKey = new Dictionary<KeyType, Delegates.Callback<ParamType1, ParamType2>>(supposedSize);
        }

        public bool Set(KeyType id, Delegates.Callback<ParamType1, ParamType2> callback)
        {
            var hasKey = callbackByKey.ContainsKey(id);
            if (callback != null)
            {
                callbackByKey[id] = callback;
            }
            else
            {
                callbackByKey.Remove(id);
            }
            return hasKey;
        }

        public bool Invoke(KeyType id, ParamType1 param1, ParamType2 param2)
        {
            var callback = (Delegates.Callback<ParamType1, ParamType2>)null;
            if (callbackByKey.TryGetValue(id, out callback))
            {
                callback(param1, param2);
                return true;
            }
            return false;
        }

        public bool InvokeSafe(KeyType id, ParamType1 param1, ParamType2 param2)
        {
            try
            {
                return Invoke(id, param1, param2);
            }
            catch (Exception e)
            {
                Logs.errorOnChannel("Nanome.Core", "DelegateMap", "Invoke error", e);
            }
            return false;
        }

        public void InvokeAll(ParamType1 param1, ParamType2 param2)
        {
            foreach (var key in callbackByKey.Keys)
            {
                callbackByKey[key](param1, param2);
            }
        }

        public void Clear()
        {
            callbackByKey.Clear();
        }

        public int GetCount()
        {
            return callbackByKey.Count;
        }

        public bool IsRegistered(KeyType id)
        {
            return callbackByKey.ContainsKey(id);
        }

    }

    public class DelegateMap<KeyType, ParamType1, ParamType2, ParamType3>
    {

        Dictionary<KeyType, Delegates.Callback<ParamType1, ParamType2, ParamType3>> callbackByKey;

        public DelegateMap(int supposedSize = 0)
        {
            callbackByKey = new Dictionary<KeyType, Delegates.Callback<ParamType1, ParamType2, ParamType3>>(supposedSize);
        }

        public bool Set(KeyType id, Delegates.Callback<ParamType1, ParamType2, ParamType3> callback)
        {
            var hasKey = callbackByKey.ContainsKey(id);
            if (callback != null)
            {
                callbackByKey[id] = callback;
            }
            else
            {
                callbackByKey.Remove(id);
            }
            return hasKey;
        }

        public bool Invoke(KeyType id, ParamType1 param1, ParamType2 param2, ParamType3 param3)
        {
            var callback = (Delegates.Callback<ParamType1, ParamType2, ParamType3>)null;
            if (callbackByKey.TryGetValue(id, out callback))
            {
                callback(param1, param2, param3);
                return true;
            }
            return false;
        }

        public bool InvokeSafe(KeyType id, ParamType1 param1, ParamType2 param2, ParamType3 param3)
        {
            try
            {
                return Invoke(id, param1, param2, param3);
            }
            catch (Exception e)
            {
                Logs.errorOnChannel("Nanome.Core", "DelegateMap", "Invoke error", e);
            }
            return false;
        }

        public void InvokeAll(ParamType1 param1, ParamType2 param2, ParamType3 param3)
        {
            foreach (var key in callbackByKey.Keys)
            {
                callbackByKey[key](param1, param2, param3);
            }
        }

        public void Clear()
        {
            callbackByKey.Clear();
        }

        public int GetCount()
        {
            return callbackByKey.Count;
        }

        public bool IsRegistered(KeyType id)
        {
            return callbackByKey.ContainsKey(id);
        }

    }

    public static class DelegatesLogic
    {

        public static Delegates.Function<bool> Not(Delegates.Function<bool> a)
        {
            if (a == null)
            {
                return null;
            }
            return delegate ()
            {
                return !a();
            };
        }

        public static Delegates.Function<bool> And(Delegates.Function<bool> a, Delegates.Function<bool> b)
        {
            return delegate ()
            {
                if (a == null || b == null)
                {
                    return false;
                }
                return (a() && b());
            };
        }

        public static Delegates.Function<bool> Or(Delegates.Function<bool> a, Delegates.Function<bool> b)
        {
            return delegate ()
            {
                if (a == null || b == null)
                {
                    return false;
                }
                return (a() || b());
            };
        }

    }

}