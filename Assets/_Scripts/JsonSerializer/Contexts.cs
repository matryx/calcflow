using UnityEngine;

using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

using Nanome.Core;
// using Nanome.Core.Buffers;
// using Nanome.Core.Extension;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class ContextSerialization
    {

        public Dictionary<string, TypeSerializer> seralizers = new Dictionary<string, TypeSerializer>();

        StringBuilder str = null;

        public ContextSerialization()
        {
            str = new StringBuilder();
        }

        ~ContextSerialization()
        {
            Close();
        }

        public void WriteGeneric(object value)
        {
            // Resolve specialized serializer from type name
            var typeName = "NULL";
            if (value != null)
            {
                typeName = value.GetType().FullName;
            }
            var typeSerializer = (TypeSerializer)null;
            if (seralizers.TryGetValue(typeName, out typeSerializer))
            {
                // Write the object content to buffer
                typeSerializer.Serialize(value, this);
                // Done here
                return;
            }
            // Unknown type
            throw new Exception("Not serializable type: " + typeName);
        }

        public void Write<T>(T part)
        {
            str.Append(part);
        }

        public override string ToString()
        {
            return str.ToString();
        }

        public void Close()
        {
        }

    }

    public class ContextDeserialization
    {

        public Dictionary<char, TypeDeserializer> deserializers = new Dictionary<char, TypeDeserializer>();

        int idx;
        string str;
        int len;

        public ContextDeserialization(string input)
        {
            idx = 0;
            str = input;
            len = input.Length;
        }

        ~ContextDeserialization()
        {
            Close();
        }

        public object ReadGeneric()
        {
            // Trim
            BurnWhitespaces();
            // Read first char
            var firstChar = str[idx];
            // Resolve deserializer
            var typeDeserializer = (TypeDeserializer)null;
            if (deserializers.TryGetValue(firstChar, out typeDeserializer))
            {
                // Read object content
                return typeDeserializer.Deserialize(this);
            }
            // Unknown char
            throw new Exception("Syntax error: " + firstChar + " (" + idx + ")");
        }

        public char ReadChar()
        {
            if (idx >= len)
            {
                return '\0';
            }
            return str[idx];
        }

        public string ReadString(int len)
        {
            return str.Substring(idx, len);
        }

        public void Burn(int nb)
        {
            idx += nb;
        }

        public void BurnWhitespaces()
        {
            while (true)
            {
                var c = ReadChar();
                if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\0')
                {
                    idx++;
                }
                else
                {
                    return;
                }
            }
        }

        public void Close()
        {
            // Nothing to do i guess?
        }

    }

}