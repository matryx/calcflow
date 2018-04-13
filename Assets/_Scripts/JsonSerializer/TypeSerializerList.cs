using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;

using Nanome.Core;
// using Nanome.Core.Buffers;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class TypeSerializerList<T> : TypeSerializer
    {

        public TypeSerializerList()
        {
        }

        public override string TypeName()
        {
            return typeof(List<T>).FullName;
        }

        public override void Serialize(object value, ContextSerialization context)
        {
            var list = (List<T>)value;
            context.Write("[");
            for (var i = 0; i < list.Count; i++)
            {
                if (i != 0)
                {
                    context.Write(",");
                }
                context.WriteGeneric(list[i]);
            }
            context.Write("]");
        }

    }

}
