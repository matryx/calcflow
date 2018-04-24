using UnityEngine;

using System;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class TypeSerializerString : TypeSerializer
    {

        public override string TypeName()
        {
            return typeof(string).FullName;
        }

        public override void Serialize(object value, ContextSerialization context)
        {
            var str = (string)value;
            context.Write("\"");
            context.Write(str);
            context.Write("\"");
        }

    }

}
