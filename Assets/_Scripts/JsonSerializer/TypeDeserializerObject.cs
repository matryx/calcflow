using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class TypeDeserializerObject : TypeDeserializer
    {

        public override object Deserialize(ContextDeserialization context)
        {
            // Result
            var res = new Dictionary<string, object>();
            // Burn bracket open, "{"
            context.Burn(1);
            // Go to first element
            context.BurnWhitespaces();
            // Loop until we hit brackend end, "}"
            var isFirst = true;
            while (context.ReadChar() != '}')
            {
                // If not first element, need to burn the ","
                if (!isFirst)
                {
                    context.Burn(1);
                    context.BurnWhitespaces();
                }
                isFirst = false;
                // Read the generic key
                var key = (string)context.ReadGeneric();
                // Go to key delimiter
                context.BurnWhitespaces();
                // Burn key delimiter, ":"
                context.Burn(1);
                // go to value
                context.BurnWhitespaces();
                // Read the generic element
                var value = context.ReadGeneric();
                // Save result
                res[key] = value;
                // Go to next stop
                context.BurnWhitespaces();
            }
            // Burn bracket end, "}"
            context.Burn(1);
            // Done, return list
            return res;
        }

    }

}
