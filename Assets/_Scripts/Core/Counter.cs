using UnityEngine;

using System;
using System.Threading;
using System.Collections;
using System.Collections.Generic;

using Nanome.Core;
using Nanome.Core.Extension;

namespace Nanome.Core
{

    public class Counter
    {

        private int value = 0;

        private int valueAdded = 0;
        private int valueSubbed = 0;

        public void Add(int nb)
        {
            Interlocked.Add(ref value, nb);
            Interlocked.Add(ref valueAdded, nb);
        }

        public void Sub(int nb)
        {
            Interlocked.Add(ref value, -nb);
            Interlocked.Add(ref valueSubbed, -nb);
        }

        public int Get()
        {
            return Interlocked.CompareExchange(ref value, 0, 0);
        }

        public int GetAdded()
        {
            return Interlocked.CompareExchange(ref valueAdded, 0, 0);
        }

        public int GetSubbed()
        {
            return Interlocked.CompareExchange(ref valueSubbed, 0, 0);
        }

        public string GetDetails()
        {
            return Get() + " ( +" + GetAdded() + " | -" + GetSubbed() + " )";
        }

        public void Clear()
        {
            Interlocked.Exchange(ref value, 0);
            Interlocked.Exchange(ref valueAdded, 0);
            Interlocked.Exchange(ref valueSubbed, 0);
        }

    }

    public class Counter<T>
    {

        object locker = new object();

        Dictionary<T, Counter> counters = new Dictionary<T, Counter>();

        void Ensure(T key)
        {
            if (!counters.ContainsKey(key))
            {
                counters[key] = new Counter();
            }
        }

        public void Add(T key, int nb)
        {
            lock (locker)
            {
                Ensure(key);
                counters[key].Add(nb);
            }
        }

        public void Sub(T key, int nb)
        {
            lock (locker)
            {
                Ensure(key);
                counters[key].Sub(nb);
            }
        }

        public int Get(T key)
        {
            lock (locker)
            {
                Ensure(key);
                return counters[key].Get();
            }
        }

        public int GetAdded(T key)
        {
            lock (locker)
            {
                Ensure(key);
                return counters[key].GetAdded();
            }
        }

        public int GetSubbed(T key)
        {
            lock (locker)
            {
                Ensure(key);
                return counters[key].GetSubbed();
            }
        }

        public string GetDetails(T key)
        {
            lock (locker)
            {
                Ensure(key);
                return counters[key].GetDetails();
            }
        }

        public List<T> Keys()
        {
            lock (locker)
            {
                return new List<T>(counters.Keys);
            }
        }

        public void Clear(T key)
        {
            lock (locker)
            {
                Ensure(key);
                counters[key].Clear();
            }
        }

        public void Clear()
        {
            lock (locker)
            {
                counters.Clear();
            }
        }

    }

}