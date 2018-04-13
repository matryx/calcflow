using UnityEngine;

using System;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class TypeDeserializerNull : TypeDeserializer
    {

        public override object Deserialize(ContextDeserialization context)
        {
            var value = context.ReadString(4);
            if (value == "null")
            {
                context.Burn(4);
                return null;
            }
            throw new Exception("Json unknown token: " + value);
        }

    }

}
