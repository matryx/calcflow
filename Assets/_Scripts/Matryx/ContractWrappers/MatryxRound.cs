using System.Collections;
using System.Collections.Generic;
using System.Numerics;
using UnityEngine;

using Nethereum.ABI.Encoders;
using Nethereum.ABI.FunctionEncoding.Attributes;
using Nethereum.ABI.FunctionEncoding;
using Nethereum.ABI.Model;
using Nethereum.Contracts;
using Nethereum.Hex.HexTypes;
using Nethereum.Hex.HexConvertors.Extensions;
using Nethereum.JsonRpc.Client;
using Nethereum.JsonRpc.UnityClient;
using Nethereum.Util;
using Nethereum.RPC.Eth.DTOs;
using Nethereum.RPC.Eth.Transactions;
using Nethereum.Signer;
using static Matryx.MatryxTournament;

namespace Matryx
{
    public class MatryxRound
    {
        private string address;

        public MatryxRound() {}
        public MatryxRound(string address)
        {
            this.address = address;
        }

        public RoundDetails Details { get; set; }

        [FunctionOutput]
        public class RoundInfo : IFunctionOutputDTO
        {
            [Parameter("bytes32[]")]
            public List<string> Submissions { get; set; }
            [Parameter("uint256")]
            public BigInteger SubmitterCount { get; set; }
            [Parameter("tuple")]
            public WinnersData Winners { get; set; }
            [Parameter("bool")]
            public bool Closed { get; set; }
        }

        [FunctionOutput]
        public class RoundDetails : IFunctionOutputDTO
        {
            [Parameter("uint256")]
            public BigInteger Start { get; set; }
            [Parameter("uint256")]
            public BigInteger Duration { get; set; }
            [Parameter("uint256")]
            public BigInteger Review { get; set; }
            [Parameter("uint256")]
            public BigInteger Bounty { get; set; }
        }
    }
}
