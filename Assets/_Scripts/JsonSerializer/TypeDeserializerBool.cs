using UnityEngine;

using System;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class TypeDeserializerBool : TypeDeserializer
    {

        public override object Deserialize(ContextDeserialization context)
        {
            var isTrue = context.ReadString(4) == "true";
            if (isTrue)
            {
                context.Burn(4);
                return true;
            }
            var isFalse = context.ReadString(5) == "false";
            if (isFalse)
            {
                context.Burn(5);
                return false;
            }
            throw new Exception("Json unknown token: " + context.ReadString(1));
        }

    }

}
