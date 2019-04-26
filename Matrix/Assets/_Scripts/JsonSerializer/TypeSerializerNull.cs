using UnityEngine;

using System;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class TypeSerializerNull : TypeSerializer
    {

        public override string TypeName()
        {
            return "NULL";
        }

        public override void Serialize(object value, ContextSerialization context)
        {
            context.Write("null");
        }

    }

}
