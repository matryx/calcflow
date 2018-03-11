using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;

namespace Nanome.Core.Daemon
{

    public class Dispatcher : Nanome.Core.Behaviour
    {

        private static readonly Queue<IEnumerator> _queue = new Queue<IEnumerator>();

        private static Dispatcher _instance;

        public static void queue(IEnumerator action)
        {
            lock (_queue)
            {
                _queue.Enqueue(action);
            }
        }

        public static void queue(Action action)
        {
            queue(_instance.actionable(action));
        }

        private void Awake()
        {
            if (_instance != null)
            {
                Destroy(gameObject);
                return;
            }
            else
            {
                _instance = this;
            }
        }

        private void Update()
        {
            lock(_queue)
            {
                while (_queue.Count > 0)
                {
                    var coroutine = _queue.Dequeue();
                    lock (coroutines)
                    {
                        coroutines.Add(coroutine);
                    }
                    _instance.StartCoroutine(coroutine);
                }
            }
        }

        private IEnumerator actionable(Action callback)
        {
            callback();
            yield return null;
        }

        private static HashSet<IEnumerator> coroutines = new HashSet<IEnumerator>();

        public delegate void StoppedDelegate();

        public static void stopAll(StoppedDelegate callback = null)
        {
            lock (_queue)
            {
                _queue.Clear();
            }
            lock (coroutines)
            {
                foreach (var coroutine in coroutines)
                {
                    _instance.StopCoroutine(coroutine);
                }
                coroutines.Clear();
            }
            if (callback != null)
            {
                callback();
            }
        }

    }

}
