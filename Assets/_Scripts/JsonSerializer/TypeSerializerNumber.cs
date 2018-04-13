using UnityEngine;

using System;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class TypeSerializerNumber<T> : TypeSerializer
    {

        public override string TypeName()
        {
            return typeof(T).FullName;
        }

        public override void Serialize(object value, ContextSerialization context)
        {
            context.Write((T)value);
        }

    }

}
