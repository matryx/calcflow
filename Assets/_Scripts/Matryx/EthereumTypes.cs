using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Nethereum.ABI.FunctionEncoding.Attributes;
using System.Numerics;

public static class EthereumTypes
{
    [FunctionOutput]
    public class Bytes32_3 : IFunctionOutputDTO
    {
        [Parameter("bytes32", "valueOne")]
        public string Value { get; set; }
        [Parameter("bytes32", "valueTwo")]
        public string ValueTwo { get; set; }
        [Parameter("bytes32", "valueThree")]
        public string ValueThree { get; set; }
    }

    [FunctionOutput]
    public class Bytes32_2 : IFunctionOutputDTO
    {
        [Parameter("bytes32", "valueOne")]
        public string Value { get; set; }
        [Parameter("bytes32", "valueTwo")]
        public string ValueTwo { get; set; }
    }

    [FunctionOutput]
    public class Address : IFunctionOutputDTO
    {
        [Parameter("address", "value", 1)]
        public string Value { get; set; }
    }

    [FunctionOutput]
    public class Bool : IFunctionOutputDTO
    {
        [Parameter("bool", "value", 1)]
        public bool Value { get; set; }
    }

    [FunctionOutput]
    public class Uint256 : IFunctionOutputDTO
    {
        [Parameter("uint256", "value", 1)]
        public BigInteger Value { get; set; }
    }

    [FunctionOutput]
    public class AddressArray : IFunctionOutputDTO
    {
        [Parameter("address[]", "value", 1)]
        public List<string> Value { get; set; }
    }
}
