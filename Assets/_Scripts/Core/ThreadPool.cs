using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;

namespace Nanome.Core.Daemon
{

    public class ThreadPool : Nanome.Core.Behaviour
    {

        private static ThreadPool _instance;

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
        }

        public delegate void JobDelegate();

        public static bool queue(JobDelegate callback)
        {
            int threadRuntime = 0;
            lock (activeLock)
            {
                threadRuntime = activeRuntime;
            }
            return System.Threading.ThreadPool.QueueUserWorkItem(delegate (object state)
            {
                int currentRuntime = 0;
                lock (activeLock)
                {
                    currentRuntime = activeRuntime;
                }
                if (threadRuntime == currentRuntime)
                {
                    callback();
                }
            });
        }

        private static object activeLock = new object();
        private static int activeRuntime = 0;

        public delegate void StoppedDelegate();

        public static void stopAll(StoppedDelegate callback = null)
        {
            // TODO ACTUALLY STOP ALL RUNNING THREADS :(
            // Or wait for them to finish
            lock (activeLock)
            {
                activeRuntime = activeRuntime + 1;
            }
        }

    }

}