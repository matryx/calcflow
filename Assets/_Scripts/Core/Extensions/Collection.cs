using UnityEngine;

using System;
using System.Linq;
using System.Collections;
using System.Collections.Generic;

namespace Nanome.Core.Extension
{

    public static class CollectionExtension
    {

        public static bool ContainsAllKeys<K, V>(this Dictionary<K, V> dictionary, params K[] keys)
        {
            foreach (var key in keys)
            {
                if (!dictionary.ContainsKey(key))
                {
                    return false;
                }
            }
            return true;
        }

        public static bool ContainsOneOfKeys<K, V>(this Dictionary<K, V> dictionary, params K[] keys)
        {
            foreach (var key in keys)
            {
                if (dictionary.ContainsKey(key))
                {
                    return true;
                }
            }
            return false;
        }

        public static bool AddOrModifyEntry<K, V>(this Dictionary<K, V> dictionary, K key, V value)
        {
            if (dictionary.ContainsKey(key))
            {
                dictionary[key] = value;
                return false;
            }
            else
            {
                dictionary.Add(key, value);
                return true;
            }
        }

        public static T[] SubArray<T>(this T[] data, int index, int length = 0)
        {
            if (length == 0)
            {
                length = data.Length;
            }
            length = Math.Min(length, data.Length - index);
            if (length < 0)
            {
                return new T[0];
            }
            var result = new T[length];
            Array.Copy(data, index, result, 0, length);
            return result;
        }

        public static List<T> SubList<T>(this List<T> data, int index, int length = 0)
        {
            if (length == 0)
            {
                length = data.Count;
            }
            length = Math.Min(length, data.Count - index);
            if (length < 0)
            {
                return new List<T>();
            }
            return data.GetRange(index, length);
        }

        public static List<T> wrapInList<T>(this T element)
        {
            var l = new List<T>();
            l.Add(element);
            return l;
        }

        public static HashSet<T> wrapInHashSet<T>(this T element)
        {
            var h = new HashSet<T>();
            h.Add(element);
            return h;
        }

        public static string Join<T>(this IEnumerable<T> l, string separator)
        {
            var stringList = l.Select(delegate (T item)
            {
                if (item == null)
                {
                    return "NULL";
                }
                return item.ToString();
            });
            return string.Join(separator, stringList.ToArray());
        }

    }

}
