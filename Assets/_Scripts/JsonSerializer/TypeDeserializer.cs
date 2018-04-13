using UnityEngine;

using System;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public abstract class TypeDeserializer
    {

        public abstract object Deserialize(ContextDeserialization context);

    }

}
