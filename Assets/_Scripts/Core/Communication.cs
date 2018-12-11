using UnityEngine;

using System;
using System.Collections.Generic;

namespace Nanome.Core
{

    public class Communication
    {

        private class ChannelHistory
        {
            public string name;
            public int messages = 0;
            public int subscribes = 0;
            public int unsubscribes = 0;
        }
        private static Dictionary<string, ChannelHistory> channelHistories = new Dictionary<string, ChannelHistory>();

        public delegate void MessageResultDelegate(object result);
        public delegate void MessageReaderDelegate(string type, object payload, MessageResultDelegate callback);
        public delegate void MessageDoneDelegate(List<object> results);

        public static List<string> history()
        {
            var lines = new List<string>();
            lock (channelHistories)
            {
                foreach (var channel in channelHistories.Values)
                {
                    lines.Add(
                        "History of channel: " + channel.name
                        + "\tmessages: " + channel.messages
                        + "\tsubscribes: " + channel.subscribes
                        + "\tunsubscribes: " + channel.unsubscribes
                    );
                }
            }
            return lines;
        }

        private class Channel
        {

            public string name;

            Dictionary<int, List<MessageReaderDelegate>> messengers;

            ChannelHistory history;

            public Channel(string channelName)
            {
                name = channelName;
                messengers = new Dictionary<int, List<MessageReaderDelegate>>();
                // History logger
                if (!channelHistories.ContainsKey(name))
                {
                    channelHistories[name] = new ChannelHistory();
                }
                history = channelHistories[name];
                history.name = name;
            }

            public void add(int id, MessageReaderDelegate callback)
            {
                lock (messengers)
                {
                    if (!messengers.ContainsKey(id))
                    {
                        messengers[id] = new List<MessageReaderDelegate>();
                    }
                    messengers[id].Add(callback);
                    history.subscribes += 1;
                }
            }

            public void clear(int id)
            {
                lock (messengers)
                {
                    if (messengers.ContainsKey(id))
                    {
                        messengers.Remove(id);
                        history.unsubscribes += 1;
                    }
                }
            }

            public void call(string type, object payload, MessageDoneDelegate callback = null)
            {
                Nanome.Core.Daemon.Dispatcher.queue(delegate ()
                {
                    var toCall = new List<MessageReaderDelegate>();
                    lock (messengers)
                    {
                        history.messages += 1;
                        foreach (var item in messengers)
                        {
                            foreach (var subscribe in item.Value)
                            {
                                toCall.Add(subscribe);
                            }
                        }
                    }
                    if (toCall.Count <= 0)
                    {
                        if (payload != null)
                        {
                            Logs.warningOnChannel("Communication", "Message on empty channel", name, "{" + type + "}", "(" + payload.GetType() + ")");
                        }
                        else
                        {
                            Logs.warningOnChannel("Communication", "Message on empty channel", name, "{" + type + "}", "(NULL)");
                        }
                    }
                    else
                    {
                        var results = new List<object>(toCall.Count);
                        var running = new Sync();
                        running.queue(toCall.Count);
                        running.onDone(delegate ()
                        {
                            if (callback != null)
                            {
                                callback(results);
                            }
                        });
                        foreach (var call in toCall)
                        {
                            try
                            {
                                call(type, payload, delegate (object result)
                                {
                                    if (result != null)
                                    {
                                        results.Add(result);
                                    }
                                    running.done();
                                });
                            }
                            catch (Exception e)
                            {
                                Logs.errorOnChannel("Communication", "Exception in messenger callback", e);
                                running.done();
                            }
                        }
                    }
                });
            }

            public int size()
            {
                lock (messengers)
                {
                    return messengers.Count;
                }
            }

        }

        private static Dictionary<int, List<Channel>> channelsBySub = new Dictionary<int, List<Channel>>();
        private static Dictionary<string, Channel> channelsByName = new Dictionary<string, Channel>();

        public static void messageToChannel(string channelName, string type, object payload = null, MessageDoneDelegate callback = null)
        {
            if (displayLogs)
            {
                Logs.timedOnChannel("Communication", "Message on", channelName, "{" + type + "}");
            }
            Channel channel = null;
            lock (channelsByName)
            {
                if (channelsByName.ContainsKey(channelName))
                {
                    channel = channelsByName[channelName];
                }
            }
            if (channel != null)
            {
                channel.call(type, payload, callback);
            }
            else
            {
                if (payload != null)
                {
                    Logs.warningOnChannel("Communication", "Message on unknown channel", channelName, "{" + type + "}", "(" + payload.GetType() + ")");
                }
                else
                {
                    Logs.warningOnChannel("Communication", "Message on unknown channel", channelName, "{" + type + "}", "(NULL)");
                }
            }
        }

        public static void listenToChannel(int id, string channelName, MessageReaderDelegate callback)
        {
            if (channelName == null || channelName.Length <= 1)
            {
                Logs.warningOnChannel("Communication", "Listening to invalid channel", channelName);
            }
            Channel channel = null;
            lock (channelsByName)
            {
                if (!channelsByName.ContainsKey(channelName))
                {
                    channelsByName[channelName] = new Channel(channelName);
                }
                channel = channelsByName[channelName];
            }
            channel.add(id, callback);
            lock (channelsBySub)
            {
                if (!channelsBySub.ContainsKey(id))
                {
                    channelsBySub[id] = new List<Channel>();
                }
                channelsBySub[id].Add(channel);
            }
        }

        public static void unlistenToChannel(int id, string channelName = null)
        {
            var emptys = new List<Channel>();
            if (channelName == null)
            {
                List<Channel> toRemove = null;
                lock (channelsBySub)
                {
                    if (channelsBySub.ContainsKey(id))
                    {
                        toRemove = channelsBySub[id];
                        channelsBySub.Remove(id);
                    }
                }
                if (toRemove != null)
                {
                    foreach (var removed in toRemove)
                    {
                        removed.clear(id);
                        if (removed.size() <= 0)
                        {
                            emptys.Add(removed);
                        }
                    }
                }
            }
            else
            {
                Channel toRemove = null;
                lock (channelsByName)
                {
                    if (channelsByName.ContainsKey(channelName))
                    {
                        toRemove = channelsByName[channelName];
                    }
                }
                if (toRemove != null)
                {
                    toRemove.clear(id);
                    if (toRemove.size() <= 0)
                    {
                        emptys.Add(toRemove);
                    }
                }
            }
            if (emptys.Count > 0)
            {
                lock (channelsByName)
                {
                    foreach (var empty in emptys)
                    {
                        if (empty.size() <= 0)
                        {
                            channelsByName.Remove(empty.name);
                        }
                    }
                }
            }
        }

        static private readonly bool displayLogs = Nanome.Core.Config.getBool("runtime-com-logs", "false");

    }

}
