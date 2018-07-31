using UnityEngine;

using System;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class TypeSerializerBool : TypeSerializer
    {

        public override string TypeName()
        {
            return typeof(bool).FullName;
        }

        public override void Serialize(object value, ContextSerialization context)
        {
            if ((bool)value)
            {
                context.Write("true");
            }
            else
            {
                context.Write("false");
            }
        }

    }

}
