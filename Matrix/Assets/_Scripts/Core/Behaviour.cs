using UnityEngine;
using System.Collections.Generic;

namespace Nanome.Core
{

    public class Behaviour : MonoBehaviour
    {

        /**
         * Monobehaviour messaging system
         */
        public delegate void SubscribeDelegate(string type, object payload);

        private class Subscribers
        {

            public string channel;
            public Dictionary<int, List<SubscribeDelegate>> subscribes;

            public Subscribers(string name)
            {
                channel = name;
                subscribes = new Dictionary<int, List<SubscribeDelegate>>();
            }

            public void add(int id, SubscribeDelegate callback)
            {
                lock (subscribes)
                {
                    if (!subscribes.ContainsKey(id))
                    {
                        subscribes[id] = new List<SubscribeDelegate>();
                    }
                    subscribes[id].Add(callback);
                }
            }

            public void clear(int id)
            {
                lock (subscribes)
                {
                    if (subscribes.ContainsKey(id))
                    {
                        subscribes.Remove(id);
                    }
                }
            }

            public void call(string type, object payload)
            {
                var toCall = new List<SubscribeDelegate>();
                lock (subscribes)
                {
                    foreach (var item in subscribes)
                    {
                        foreach (var subscribe in item.Value)
                        {
                            toCall.Add(subscribe);
                        }
                    }
                }
                if (toCall.Count <= 0)
                {
                    //Logs.debug("Publish on empty channel", "\"" + channel + "\"", "<" + type + ">", "(" + payload.GetType() + ")");
                }
                else
                {
                    Nanome.Core.Daemon.Dispatcher.queue(delegate ()
                    {
                        foreach (var call in toCall)
                        {
                            call(type, payload);
                        }
                    });
                }
            }

            public int size()
            {
                lock (subscribes)
                {
                    return subscribes.Count;
                }
            }

        }

        private static Dictionary<int, List<Subscribers>> channelsBySub = new Dictionary<int, List<Subscribers>>();
        private static Dictionary<string, Subscribers> channelsByKey = new Dictionary<string, Subscribers>();

        public void publish(string channel, string type, object payload = null)
        {
            //Logs.debug("Publishing on", "\"" + channel + "\"", "<" + type + ">");
            Subscribers subs = null;
            lock (channelsByKey)
            {
                if (channelsByKey.ContainsKey(channel))
                {
                    subs = channelsByKey[channel];
                }
            }
            if (subs != null)
            {
                subs.call(type, payload);
            }
            else
            {
                if (payload != null)
                {
                    //Logs.debug("Publish on unknown channel", "\"" + channel + "\"", "<" + type + ">", "(" + payload.GetType() + ")");
                }
                else
                {
                    //Logs.debug("Publish on unknown channel", "\"" + channel + "\"", "<" + type + ">", "(NULL)");
                }
            }
        }

        public void subscribe(string channel, SubscribeDelegate callback)
        {
            var sub = GetInstanceID();
            Subscribers subs = null;
            lock (channelsByKey)
            {
                if (!channelsByKey.ContainsKey(channel))
                {
                    channelsByKey[channel] = new Subscribers(channel);
                }
                subs = channelsByKey[channel];
            }
            subs.add(sub, callback);
            lock (channelsBySub)
            {
                if (!channelsBySub.ContainsKey(sub))
                {
                    channelsBySub[sub] = new List<Subscribers>();
                }
                channelsBySub[sub].Add(subs);
            }
        }

        public void unsubscribe(string channel = null)
        {
            var emptys = new List<Subscribers>();
            var sub = GetInstanceID();
            if (channel == null)
            {
                List<Subscribers> toRemove = null;
                lock (channelsBySub)
                {
                    if (channelsBySub.ContainsKey(sub))
                    {
                        toRemove = channelsBySub[sub];
                        channelsBySub.Remove(sub);
                    }
                }
                if (toRemove != null)
                {
                    foreach (var removed in toRemove)
                    {
                        removed.clear(sub);
                        if (removed.size() <= 0)
                        {
                            emptys.Add(removed);
                        }
                    }
                }
            }
            else
            {
                Subscribers toRemove = null;
                lock (channelsByKey)
                {
                    if (channelsByKey.ContainsKey(channel))
                    {
                        toRemove = channelsByKey[channel];
                    }
                }
                if (toRemove != null)
                {
                    toRemove.clear(sub);
                    if (toRemove.size() <= 0)
                    {
                        emptys.Add(toRemove);
                    }
                }
            }
            if (emptys.Count > 0)
            {
                lock (channelsByKey)
                {
                    foreach (var empty in emptys)
                    {
                        if (empty.size() <= 0)
                        {
                            channelsByKey.Remove(empty.channel);
                        }
                    }
                }
            }
        }

        //protected GameObject addObject(string name)
        //{
        //    return Nanome.Core.Make.gameObject(name, gameObject.transform);
        //}

        //protected B addBuilder<B>(string name = null, object options = null) where B : Nanome.Behaviour.Builder
        //{
        //    var obj = Nanome.Core.Make.objectWithComponent<B>(gameObject.transform);
        //    var builder = obj.GetComponent<B>();
        //    if (name != null)
        //    {
        //        obj.name = name;
        //    }
        //    if (options != null)
        //    {
        //        builder.setParams(options);
        //    }
        //    return builder;
        //}

        //protected S addScript<S>() where S : Component
        //{
        //    return gameObject.AddComponent(typeof(S)) as S;
        //}

        //protected GameObject addPrimitiveRenderer(string name, PrimitiveType type)
        //{
        //    var lu = GameObject.CreatePrimitive(type);
        //    lu.name = name;
        //    Destroy(lu.GetComponent<Rigidbody>());
        //    Destroy(lu.GetComponent<Collider>());
        //    lu.transform.parent = this.transform;
        //    return lu;
        //}

        //protected GameObject addText(string name, string value, float scale, string halign = "center", string valign = "middle")
        //{
        //    var obj = Nanome.Core.Make.objectWithComponent<TMPro.TextMeshPro>(gameObject.transform);
        //    obj.name = name;
        //    doTextAlign(obj, halign, valign);
        //    var txt = obj.GetComponent<TMPro.TextMeshPro>();
        //    txt.text = value;
        //    obj.transform.localScale = new Vector3(-1, 1, 1) * (scale / 20f);
        //    var rect = obj.GetComponent<RectTransform>();
        //    rect.sizeDelta = new Vector2(1000f, rect.sizeDelta.y);
        //    return obj;
        //}

        //protected GameObject addBillboardText(string name, string value, float scale, string halign = "center", string valign = "middle")
        //{
        //    var obj = Nanome.Core.Make.objectWithComponent<TMPro.TextMeshPro>(gameObject.transform);
        //    obj.transform.localScale = new Vector3(-1, 1, 1) * (scale / 20f);
        //    doTextAlign(obj, halign, valign);
        //    var billboard = obj.AddComponent<Nanome.Behaviour.Billboard>();
        //    billboard.tilt = -Vector3.forward;
        //    obj.name = name;
        //    var txt = obj.GetComponent<TMPro.TextMeshPro>();
        //    txt.text = value;
        //    var rect = obj.GetComponent<RectTransform>();
        //    rect.sizeDelta = new Vector2(1000f, rect.sizeDelta.y);
        //    return obj;
        //}

        //protected void doTextAlign(GameObject txtObj, string halign, string valign)
        //{
        //    var txt = txtObj.GetComponent<TMPro.TextMeshPro>();
        //    var rect = txtObj.GetComponent<RectTransform>();
        //    Vector2 pivot = rect.pivot;
        //    TMPro.TextAlignmentOptions align = TMPro.TextAlignmentOptions.Center;
        //    if (halign == "middle" || halign == "center")
        //    {
        //        pivot.x = 0.5f;
        //        align = TMPro.TextAlignmentOptions.Center;
        //    }
        //    if (halign == "right")
        //    {
        //        pivot.x = 1f;
        //        align = TMPro.TextAlignmentOptions.Right;
        //    }
        //    if (halign == "left")
        //    {
        //        pivot.x = 0f;
        //        align = TMPro.TextAlignmentOptions.Left;
        //    }
        //    if (valign == "middle" || valign == "center")
        //    {
        //        pivot.y = 0.5f;
        //    }
        //    if (halign == "top")
        //    {
        //        pivot.x = 1f;
        //    }
        //    if (halign == "bottom")
        //    {
        //        pivot.x = 0f;
        //    }
        //    txt.alignment = align;
        //    rect.pivot = pivot;
        //}

        public void OnDestroy()
        {
            unsubscribe();
        }

        protected List<T> FindAllInScene<T>(bool includeInactives = true) where T : UnityEngine.Component
        {
            var founds = new List<T>();
            foreach (var root in UnityEngine.SceneManagement.SceneManager.GetActiveScene().GetRootGameObjects())
            {
                foreach (var found in root.GetComponentsInChildren<T>(includeInactives))
                {
                    founds.Add(found);
                }
            }
            return founds;
        }

    }

}
