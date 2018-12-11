using System;

using UnityEngine;

using Nanome.Core.Extension;

namespace Nanome.Core
{

    public class ThrottleConcurrent
    {

        public delegate void TaskDelegate(DoneDelegate done);
        public delegate void DoneDelegate();

        object lockConcurrent;
        int maxConcurrent;
        int nowConcurrent;

        public ThrottleConcurrent(int max = 1)
        {
            lockConcurrent = new object();
            maxConcurrent = max;
            nowConcurrent = 0;
        }

        public void start(TaskDelegate task)
        {
            lock (lockConcurrent)
            {
                if (nowConcurrent >= maxConcurrent)
                {
                    return; // Skip when too much going on
                }
                nowConcurrent += 1;
            }
            task(markDone);
        }

        void markDone()
        {
            lock (lockConcurrent)
            {
                nowConcurrent -= 1;
            }
        }

        public int runnings()
        {
            lock (lockConcurrent)
            {
                return nowConcurrent;
            }
        }

    }

    public class ThrottleTimer
    {

        public delegate void TaskDelegate();

        object lockTimer;
        double nextTimer;
        double distTimer;

        bool main;

        TaskDelegate lastTask;

        public ThrottleTimer(double seconds = 0.5f, bool startImmediatly = true, bool onMainThread = true)
        {
            main = onMainThread;
            lockTimer = new object();
            distTimer = seconds;
            if (startImmediatly)
            {
                nextTimer = -1000f;
            }
            else
            {
                var time = now();
                nextTimer = time + distTimer;
            }
        }

        public void trigger(TaskDelegate task)
        {
            lock (lockTimer)
            {
                var time = now();
                if (time <= nextTimer)
                {
                    lastTask = task;
                    return;
                }
                lastTask = null;
                nextTimer = time + distTimer;
            }
            task();
        }

        public void runLast()
        {
            lock (lockTimer)
            {
                if (lastTask != null)
                {
                    lastTask();
                    lastTask = null;
                }
            }
        }

        public void delay(double seconds)
        {
            distTimer = seconds;
        }

        public double delay()
        {
            return distTimer;
        }

        public bool triggerable()
        {
            lock (lockTimer)
            {
                var time = now();
                if (time <= nextTimer)
                {
                    return false;
                }
                return true;
            }
        }

        public void reset()
        {
            lock (lockTimer)
            {
                var time = now();
                nextTimer = time + distTimer;
            }
        }

        public bool triggerOrPass()
        {
            lock (lockTimer)
            {
                var time = now();
                if (time <= nextTimer)
                {
                    return false;
                }
                nextTimer = time + distTimer;
                return true;
            }
        }

        double now()
        {
            if (main)
            {
                return Time.time;
            }
            else
            {
                return DateTime.Now.ToTimestamp();
            }
        }

    }

    public class ThrottleFrame
    {

        bool available = true;

        public ThrottleFrame()
        {
        }

        public void trigger(Delegates.Callback task)
        {
            if (!available)
            {
                return;
            }
            available = false;
            Nanome.Core.Daemon.Dispatcher.queue(delegate ()
            {
                task();
                available = true;
            });
        }

    }

    public class ThrottleCount
    {

        public delegate void TaskDelegate();

        int remaining = 0;

        public ThrottleCount(int allowedCount = 1)
        {
            remaining = allowedCount;
        }

        public void trigger(TaskDelegate task)
        {
            if (remaining > 0)
            {
                remaining -= 1;
                task();
            }
        }

        public void allowed(int allowedCount)
        {
            remaining = allowedCount;
        }

        public bool finished()
        {
            return remaining <= 0;
        }

    }

}
