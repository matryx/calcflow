using UnityEngine;

using System;
using System.Text;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class TypeDeserializerString : TypeDeserializer
    {

        public override object Deserialize(ContextDeserialization context)
        {
            // Start building the string
            var res = new StringBuilder();
            // Read opening char, " or '
            var open = context.ReadChar();
            // Burn opening char
            context.Burn(1);
            // Read first string char
            var curr = context.ReadChar();
            // Until we find the opening char again
            var lastIsBackslash = false;
            while (curr != open || lastIsBackslash)
            {
                if (lastIsBackslash)
                {
                    if (curr == '\"')
                    {
                        res.Append('\\');
                        res.Append('\"');
                    }
                    if (curr == '\'')
                    {
                        res.Append('\\');
                        res.Append('\'');
                    }
                    if (curr == 't')
                    {
                        res.Append('\\');
                        res.Append('\t');
                    }
                    if (curr == 'r')
                    {
                        res.Append('\\');
                        res.Append('\r');
                    }
                    if (curr == 'n')
                    {
                        res.Append('\\');
                        res.Append('\n');
                    }
                    lastIsBackslash = false;
                }
                else
                {
                    if (curr == '\\')
                    {
                        lastIsBackslash = true;
                    }
                    else
                    {
                        // Append char to string
                        res.Append(curr);
                    }
                }
                // Burn read char
                context.Burn(1);
                // Read next to be tested
                curr = context.ReadChar();
            }
            // Burn closing char
            context.Burn(1);
            // Done
            return res.ToString();
        }

    }

}