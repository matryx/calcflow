using UnityEngine;

using System;
using System.Threading;
using System.Collections;
using System.Collections.Generic;

using Nanome.Core.Extension;

namespace Nanome.Core
{

    public class Async
    {

        // Instance counter
        static public Counter instances = new Counter();

        // Usefull delegates
        public delegate IEnumerator CoroutineDelegate(Async async, object param);
        public delegate IEnumerator SimpleCoroutineDelegate();
        public delegate void ActionDelegate(Async async);
        public delegate void SimpleActionDelegate();

        public delegate void EventDelegate(object data);

        public delegate void StartDelegate();
        public delegate void DoneDelegate();

        /**
         * Async class properties/content
         */
        protected bool isMain = false;
        protected Async proxy = null;

        private object locker = new object();
        private Dictionary<string, List<EventDelegate>> events;
        private Dictionary<string, Queue<object>> eventHistory;

        private static Thread s_MainThread = Thread.CurrentThread;

        private Async(bool isMain)
        {
            this.isMain = isMain;
            this.events = new Dictionary<string, List<EventDelegate>>(1);
            this.eventHistory = new Dictionary<string, Queue<object>>(1);
            instances.Add(1);
        }

        ~Async()
        {
            instances.Sub(1);
        }

        public bool runningOnMainThread()
        {
            return isMain;
        }

        public void pushEvent(string type, object data)
        {
            // Error if no associated async
            if (proxy == null)
            {
                return;
            }
            // Events callback not supported on threads
            if (!proxy.isMain)
            {
                return;
            }
            // Locking
            lock (proxy.locker)
            {
                // We use events when possible
                var callbacks = (List<EventDelegate>)null;
                if (proxy.events.TryGetValue(type, out callbacks))
                {
                    var calls = new List<EventDelegate>(callbacks);
                    Nanome.Core.Daemon.Dispatcher.queue(delegate ()
                    {
                        if (calls != null)
                        {
                            for (int i = 0; i < calls.Count; i++)
                            {
                                calls[i](data);
                            }
                        }
                    });
                }
                // We store datas if no event were specified
                if (!proxy.eventHistory.ContainsKey(type))
                {
                    proxy.eventHistory[type] = new Queue<object>(1);
                }
                proxy.eventHistory[type].Enqueue(data);
            }
        }

        public void onEvent(string type, EventDelegate fn)
        {
            // Events callback not supported on threads
            if (!isMain)
            {
                return;
            }
            // Locking
            lock (locker)
            {
                // Register event
                if (!events.ContainsKey(type))
                {
                    events[type] = new List<EventDelegate>(1);
                }
                events[type].Add(fn);
                // If events of this type already happened, trigger them
                var previousEvents = (Queue<object>)null;
                if (eventHistory.TryGetValue(type, out previousEvents))
                {
                    while (previousEvents.Count > 0)
                    {
                        fn(previousEvents.Dequeue());
                    }
                }
            }
        }

        public void onStart(StartDelegate fn)
        {
            onEvent("ASYNC-START", delegate (object a)
            {
                fn();
            });
        }

        public void onStartThreaded(StartDelegate fn)
        {
            onEvent("ASYNC-START-THREADED", delegate (object a)
            {
                Async.runInThread(delegate ()
                {
                    fn();
                });
            });
        }

        public void onDone(DoneDelegate fn)
        {
            onEvent("ASYNC-DONE", delegate (object a)
            {
                fn();
            });
        }

        public void onDoneThreaded(DoneDelegate fn)
        {
            onEvent("ASYNC-DONE-THREADED", delegate (object a)
            {
                Async.runInThread(delegate ()
                {
                    fn();
                });
            });
        }

        static int counter = 0;
        static object counterLock = new object();
        static double firstTime = 0.0f;

        /**
         * Easy to use async interface
         */
        public static void runInThread(SimpleActionDelegate fn)
        {
            runInThread(delegate (Async unused)
            {
                fn();
            });
        }
        public static Async runInThread(ActionDelegate fn)
        {
            // If we are debugging, run threads in the main thread
            if (debugInMain)
            {
                return runInMain(fn);
            }
            // Otherwise proceed normally in the threadpool
            Async mainThread = new Async(true);
            Async poolThread = new Async(false);
            mainThread.proxy = poolThread;
            poolThread.proxy = mainThread;
            // Count thread scheduling
            lock (counterLock)
            {
                counter++;
                if (debugPlots)
                {
                    if (firstTime == 0f)
                    {
                        firstTime = DateTime.Now.ToTimestamp();
                    }
                    DataPlotter.AddDataPoint("Asyncs", "Async", (float)(DateTime.Now.ToTimestamp() - firstTime), counter);
                }
            }
            // Don't run the thread if things are going out of hand
            var running = true;
            lock (counterLock)
            {
                running = counter < 100;
            }
            if (running)
            {
                // Schedule thread
                Nanome.Core.Daemon.ThreadPool.queue(delegate ()
                {
                    poolThread.pushEvent("ASYNC-START", null);
                    poolThread.pushEvent("ASYNC-START-THREADED", null);
                    try
                    {
                        fn(poolThread);
                    }
                    catch (Exception exc)
                    {
                        Logs.errorOnChannel("Nanome.Core", "Error in a thread", exc);
                    }
                    poolThread.pushEvent("ASYNC-DONE", null);
                    poolThread.pushEvent("ASYNC-DONE-THREADED", null);
                    lock (counterLock)
                    {
                        counter--;
                        if (debugPlots)
                        {
                            DataPlotter.AddDataPoint("Asyncs", "Async", (float)(DateTime.Now.ToTimestamp() - firstTime), counter);
                        }
                    }
                });
            }
            else
            {
                Logs.errorOnChannel("Nanome.Core", "Too much threading load", "bailing out");
            }
            return mainThread;
        }

        public static Async runInMain(ActionDelegate fn)
        {
            Async main1Thread = new Async(true);
            Async main2Thread = new Async(true);
            main1Thread.proxy = main2Thread;
            main2Thread.proxy = main1Thread;
            Nanome.Core.Daemon.Dispatcher.queue(delegate ()
            {
                main2Thread.pushEvent("ASYNC-START", null);
                main2Thread.pushEvent("ASYNC-START-THREADED", null);
                try
                {
                    fn(main2Thread);
                }
                catch (Exception exc)
                {
                    Logs.errorOnChannel("Nanome.Core", "Error in main thread", exc);
                }
                main2Thread.pushEvent("ASYNC-DONE", null);
                main2Thread.pushEvent("ASYNC-DONE-THREADED", null);
            });
            return main1Thread;
        }
        public static void runInMain(SimpleActionDelegate fn)
        {
            runInMain(delegate (Async unused)
            {
                fn();
            });
        }

        public static void runInMainIfNeeded(SimpleActionDelegate fn)
        {
            if (Thread.CurrentThread == s_MainThread)
            {
                fn();
                return;
            }
            runInMain(fn);
        }

        public static Async runInCoroutine(CoroutineDelegate fn, object datas = null)
        {
            Async main1Thread = new Async(true);
            Async main2Thread = new Async(true);
            main1Thread.proxy = main2Thread;
            main2Thread.proxy = main1Thread;
            Nanome.Core.Daemon.Dispatcher.queue(fn(main2Thread, datas));
            return main1Thread;
        }
        public static void runInCoroutine(SimpleCoroutineDelegate fn)
        {
            runInCoroutine(delegate (Async unused1, object unused2)
            {
                return fn();
            });
        }

        // Static config
        static readonly bool debugPlots = Nanome.Core.Config.getBool("runtime-plot-async", "false");
        static readonly bool debugInMain = Nanome.Core.Config.getBool("runtime-single-thread", "false"); // Force running everything in a thread

    }

}