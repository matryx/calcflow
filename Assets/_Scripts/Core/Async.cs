using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;

namespace Nanome.Core
{

    public class Async
    {

        // Usefull delegates
        public delegate IEnumerator CoroutineDelegate(Async async);
        public delegate void ActionDelegate(Async async);
        public delegate void EventDelegate(object data);

        /**
         * Async class properties/content
         */
        protected bool isMain = false;
        public bool success = false;
        protected Async proxy = null;

        private Dictionary<string, List<object>> datas;
        private Dictionary<string, List<EventDelegate>> events;

        private Async(bool isMain)
        {
            this.isMain = isMain;
            this.events = new Dictionary<string, List<EventDelegate>>();
            this.datas = new Dictionary<string, List<object>>();
        }

        public Async()
        {
        }

        public void pushEvent(string type, object data)
        {
            // Error if no associated async
            if (proxy == null)
            {
                return;
            }
            // We use events when possible
            lock (proxy.events)
            {
                if (proxy.events.ContainsKey(type))
                {
                    if (!proxy.isMain)
                    {
                        // return; // Events callbacks not supported on threads (use datas instead)
                    }
                    else
                    {
                        Nanome.Core.Daemon.Dispatcher.queue(delegate ()
                        {
                            List<EventDelegate> calls;
                            lock (proxy.events)
                            {
                                calls = proxy.events[type];
                            }
                            if (calls != null)
                            {
                                for (int i = 0; i < calls.Count; i++)
                                {
                                    calls[i](data);
                                }
                            }
                        });
                        return;
                    }
                }
            }
            // We store datas if no event were specified
            lock (proxy.datas)
            {
                if (!proxy.datas.ContainsKey(type))
                {
                    proxy.datas[type] = new List<object>();
                }
                proxy.datas[type].Add(data);
            }
            return;
        }

        public object recvEvent(string type)
        {
            object data = null;
            lock (this.datas)
            {
                if (this.datas.ContainsKey(type))
                {
                    if (this.datas[type].Count > 0)
                    {
                        data = this.datas[type][0];
                        this.datas[type].RemoveAt(0);
                    }
                }
            }
            return data;
        }

        public void onEvent(string type, EventDelegate fn)
        {
            if (!isMain)
            {
                return; // Not supported on threads
            }
            lock (this.events)
            {
                if (!this.events.ContainsKey(type))
                {
                    this.events[type] = new List<EventDelegate>();
                }
                this.events[type].Add(fn);
            }
        }

        /**
         * Easy to use async interface
         */
        public static Async runInThread(ActionDelegate fn)
        {
            Async mainThread = new Async(true);
            Async poolThread = new Async(false);
            mainThread.proxy = poolThread;
            poolThread.proxy = mainThread;
            var success = Nanome.Core.Daemon.ThreadPool.queue(delegate ()
            {
                try
                {
                    fn(poolThread);
                }
                catch (Exception exc)
                {
                    Debug.Log("Error in a thread: " + exc.ToString());
                }
            });
            mainThread.success = success;
            poolThread.success = success;
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
                fn(main2Thread);
            });
            main1Thread.success = true;
            main2Thread.success = true;
            return main1Thread;
        }

        public static Async runInCoroutine(CoroutineDelegate fn)
        {
            Async main1Thread = new Async(true);
            Async main2Thread = new Async(true);
            main1Thread.proxy = main2Thread;
            main2Thread.proxy = main1Thread;
            Nanome.Core.Daemon.Dispatcher.queue(fn(main2Thread));
            main1Thread.success = true;
            main2Thread.success = true;
            return main1Thread;
        }

    }

}