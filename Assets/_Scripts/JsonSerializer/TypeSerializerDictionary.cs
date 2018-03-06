using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;

using Nanome.Core;
// using Nanome.Core.Buffers;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class TypeSerializerDictionary<K, V> : TypeSerializer
    {

        public TypeSerializerDictionary()
        {
        }

        public override string TypeName()
        {
            return typeof(Dictionary<K, V>).FullName;
        }

        public override void Serialize(object obj, ContextSerialization context)
        {
            var dict = (Dictionary<K, V>)obj;
            context.Write("{");
            var isFirst = true;
            foreach (var item in dict)
            {
                if (!isFirst)
                {
                    context.Write(",");
                }
                isFirst = false;
                var key = item.Key;
                var value = item.Value;
                context.Write("\"");
                context.Write(key.ToString());
                context.Write("\":");
                context.WriteGeneric(value);
            }
            context.Write("}");
        }

    }

}
