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
using Nethereum.Util;
using Nethereum.RPC.Eth.DTOs;
using Nethereum.RPC.Eth.Transactions;
using Nethereum.Signer;
using static Matryx.MatryxTournament;
using System;

namespace Matryx
{
    public class MatryxRound
    {
        private string address;

        public MatryxRound() {}
        public MatryxRound(int index)
        {
            this.index = index;
        }

        public RoundInfo Info { get; set; }
        public RoundDetails Details { get; set; }

        public string status;
        public DateTime startTime;
        public DateTime endTime;

        public static readonly string STATE_SCHEDULED = "scheduled";
        public static readonly string STATE_UNFUNDED = "notFunded";
        public static readonly string STATE_OPEN = "open";
        public static readonly string STATE_INREVIEW = "inReview";
        public static readonly string STATE_HASWINNERS = "hasWinners";
        public static readonly string STATE_CLOSED = "closed";

        public string TimeRemaining
        {
            get
            {
                TimeSpan timeRemaining = endTime - DateTime.Now;
                if (timeRemaining.TotalSeconds < 0) { return "Closed"; }
                var unitLabels = new string[] { "Sec", "Min", "Hrs", " Days", "Wks", "Yrs" };
                var amountPerNextUnit = new double[] { 60, 60, 24, 7, 54 };

                var amount = timeRemaining.TotalSeconds;
                int i = 0;
                for(; i < unitLabels.Length; i++)
                {
                    if (amount < amountPerNextUnit[i]) break;
                    amount = amount / amountPerNextUnit[i];
                }

                return Math.Truncate(amount) + unitLabels[i];
            }
        }

        public int index;
        public int Number { get { return index + 1; } }
        public int totalParticipants;
        public int totalSubmissions;
        public List<MatryxSubmission> winningSubmissions;
        public decimal Bounty
        {
            get
            {
                return (decimal)(Details.Bounty / new BigInteger((decimal)1e18));
            }
            set
            {
                Details.Bounty = new BigInteger(value * (decimal)1e18);
            }
        }

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
