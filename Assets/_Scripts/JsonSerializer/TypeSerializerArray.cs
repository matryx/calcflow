using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;

using Nanome.Core;
// using Nanome.Core.Buffers;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class TypeSerializerArray<T> : TypeSerializer
    {

        public TypeSerializerArray()
        {
        }

        public override string TypeName()
        {
            return typeof(T[]).FullName;
        }

        public override void Serialize(object value, ContextSerialization context)
        {
            var arr = (T[])value;
            context.Write("[");
            for (var i = 0; i < arr.Length; i++)
            {
                if (i != 0)
                {
                    context.Write(",");
                }
                context.WriteGeneric(arr[i]);
            }
            context.Write("]");
        }

    }

}
