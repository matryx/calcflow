using UnityEngine;

using System;
using System.Threading;
using System.Collections.Generic;

namespace Nanome.Core
{

    public class Sync
    {

        static public Counter instances = new Counter();

        public delegate void DoneDelegate();

        int todos;
        List<DoneDelegate> dones;

        bool hasFinished = false;
        bool hasTriggered = false;

        public Sync()
        {
            todos = 0;
            dones = new List<DoneDelegate>();
            instances.Add(1);
        }

        ~Sync()
        {
            instances.Sub(1);
        }

        public void queue(int nb = 1)
        {
            edit(nb);
            done(0); // Call done, just in case the asked number is zero and we are done
        }

        public void done(int nb = 1)
        {
            if (hasFinished)
            {
                return;
            }
            edit(-nb);
            if (read() <= 0)
            {
                hasFinished = true;
                Nanome.Core.Daemon.Dispatcher.queue(delegate ()
                {
                    lock (dones)
                    {
                        if (hasTriggered)
                        {
                            return;
                        }
                        foreach (var callback in dones)
                        {
                            callback();
                        }
                        dones.Clear();
                        hasTriggered = true;
                    }
                });
            }
        }

        public void onDone(DoneDelegate callback)
        {
            lock (dones)
            {
                if (hasTriggered)
                {
                    callback();
                }
                else
                {
                    dones.Add(callback);
                }
            }
        }

        int read()
        {
            return Interlocked.CompareExchange(ref todos, 0, 0);
        }
        void edit(int offset)
        {
            Interlocked.Add(ref todos, offset);
        }

    }

}