using UnityEngine;

using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

using Nanome.Core;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class Serializer
    {

        Dictionary<string, TypeSerializer> seralizers = new Dictionary<string, TypeSerializer>();
        Dictionary<char, TypeDeserializer> deserializers = new Dictionary<char, TypeDeserializer>();

        public Serializer()
        {
            // Simple types serializers
            AddTypeSerializer(new TypeSerializerNull());
            AddTypeSerializer(new TypeSerializerBool());
            AddTypeSerializer(new TypeSerializerString());
            AddTypeSerializer(new TypeSerializerNumber<byte>());
            AddTypeSerializer(new TypeSerializerNumber<short>());
            AddTypeSerializer(new TypeSerializerNumber<int>());
            AddTypeSerializer(new TypeSerializerNumber<long>());
            AddTypeSerializer(new TypeSerializerNumber<float>());
            AddTypeSerializer(new TypeSerializerNumber<double>());
            // Generic types serializers
            AddTypeSerializer(new TypeSerializerArray<object>());
            AddTypeSerializer(new TypeSerializerArray<byte>());
            AddTypeSerializer(new TypeSerializerArray<bool>());
            AddTypeSerializer(new TypeSerializerArray<short>());
            AddTypeSerializer(new TypeSerializerArray<int>());
            AddTypeSerializer(new TypeSerializerArray<long>());
            AddTypeSerializer(new TypeSerializerArray<float>());
            AddTypeSerializer(new TypeSerializerArray<double>());
            AddTypeSerializer(new TypeSerializerArray<string>());
            AddTypeSerializer(new TypeSerializerList<object>());
            AddTypeSerializer(new TypeSerializerList<byte>());
            AddTypeSerializer(new TypeSerializerList<bool>());
            AddTypeSerializer(new TypeSerializerList<short>());
            AddTypeSerializer(new TypeSerializerList<int>());
            AddTypeSerializer(new TypeSerializerList<long>());
            AddTypeSerializer(new TypeSerializerList<float>());
            AddTypeSerializer(new TypeSerializerList<double>());
            AddTypeSerializer(new TypeSerializerList<string>());
            AddTypeSerializer(new TypeSerializerDictionary<string, object>());
            AddTypeSerializer(new TypeSerializerDictionary<string, bool>());
            AddTypeSerializer(new TypeSerializerDictionary<string, short>());
            AddTypeSerializer(new TypeSerializerDictionary<string, int>());
            AddTypeSerializer(new TypeSerializerDictionary<string, long>());
            AddTypeSerializer(new TypeSerializerDictionary<string, float>());
            AddTypeSerializer(new TypeSerializerDictionary<string, string>());
            AddTypeSerializer(new TypeSerializerDictionary<int, object>());
            AddTypeSerializer(new TypeSerializerDictionary<int, bool>());
            AddTypeSerializer(new TypeSerializerDictionary<int, short>());
            AddTypeSerializer(new TypeSerializerDictionary<int, int>());
            AddTypeSerializer(new TypeSerializerDictionary<int, long>());
            AddTypeSerializer(new TypeSerializerDictionary<int, float>());
            AddTypeSerializer(new TypeSerializerDictionary<int, string>());
            AddTypeSerializer(new TypeSerializerDictionary<long, object>());
            AddTypeSerializer(new TypeSerializerDictionary<long, bool>());
            AddTypeSerializer(new TypeSerializerDictionary<long, short>());
            AddTypeSerializer(new TypeSerializerDictionary<long, int>());
            AddTypeSerializer(new TypeSerializerDictionary<long, long>());
            AddTypeSerializer(new TypeSerializerDictionary<long, float>());
            AddTypeSerializer(new TypeSerializerDictionary<long, string>());
            // Adding some basic tuples
            // AddTypeSerializer(new TypeSerializerTuple<string, string>());
            // AddTypeSerializer(new TypeSerializerTuple<string, int>());
            // AddTypeSerializer(new TypeSerializerTuple<string, long>());
            // AddTypeSerializer(new TypeSerializerTuple<int, string>());
            // AddTypeSerializer(new TypeSerializerTuple<int, int>());
            // AddTypeSerializer(new TypeSerializerTuple<int, long>());
            // AddTypeSerializer(new TypeSerializerTuple<long, string>());
            // AddTypeSerializer(new TypeSerializerTuple<long, int>());
            // AddTypeSerializer(new TypeSerializerTuple<long, long>());

            // Deserializers
            var nulls = new TypeDeserializerNull();
            var numbers = new TypeDeserializerNumber();
            var bools = new TypeDeserializerBool();
            var strings = new TypeDeserializerString();
            var objects = new TypeDeserializerObject();
            var arrays = new TypeDeserializerArray();
            AddTypeDeserializer('{', objects);
            AddTypeDeserializer('[', arrays);
            AddTypeDeserializer('n', nulls);
            AddTypeDeserializer('t', bools);
            AddTypeDeserializer('f', bools);
            AddTypeDeserializer('\'', strings);
            AddTypeDeserializer('"', strings);
            AddTypeDeserializer('-', numbers);
            AddTypeDeserializer('+', numbers);
            AddTypeDeserializer('0', numbers);
            AddTypeDeserializer('1', numbers);
            AddTypeDeserializer('2', numbers);
            AddTypeDeserializer('3', numbers);
            AddTypeDeserializer('4', numbers);
            AddTypeDeserializer('5', numbers);
            AddTypeDeserializer('6', numbers);
            AddTypeDeserializer('7', numbers);
            AddTypeDeserializer('8', numbers);
            AddTypeDeserializer('9', numbers);
            AddTypeDeserializer('.', numbers);
            //AddTypeDeserializer('e', numbers);

        }

        public byte[] Serialize(object obj)
        {
            var str = SerializeString(obj);
            return Encoding.UTF8.GetBytes(str);
        }

        public string SerializeString(object obj)
        {
            var context = new ContextSerialization();
            context.seralizers = seralizers;
            context.WriteGeneric(obj);
            var result = context.ToString();
            context.Close();
            return result;
        }

        public T Deserialize<T>(byte[] data)
        {
            var str = Encoding.UTF8.GetString(data);
            return DeserializeString<T>(str);
        }

        public T DeserializeString<T>(string str)
        {
            var context = new ContextDeserialization(str);
            context.deserializers = deserializers;
            var result = (T)context.ReadGeneric();
            context.Close();
            return result;
        }

        public void AddTypeSerializer(TypeSerializer typeSerializer)
        {
            var typeName = typeSerializer.TypeName();
            seralizers[typeName] = typeSerializer;
        }

        public void AddTypeDeserializer(char firstChar, TypeDeserializer typeDeserializer)
        {
            deserializers[firstChar] = typeDeserializer;
        }

    }

}
