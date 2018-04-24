using UnityEngine;

using System;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public abstract class TypeSerializer
    {

        public abstract string TypeName();

        public abstract void Serialize(object value, ContextSerialization context);

    }

}
