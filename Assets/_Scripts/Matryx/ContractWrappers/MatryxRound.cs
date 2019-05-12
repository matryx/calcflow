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
        public MatryxTournament tournament;

        public MatryxRound() {}
        public MatryxRound(int index)
        {
            this.index = index;
        }
        public MatryxRound(int index, RoundDetails details) : this(index)
        {
            Details = details;
            startDate = Utils.Time.FromUnixTime(details.Start);
            endDate = Utils.Time.FromUnixTime(details.Start + Details.Duration);
            reviewEndDate = Utils.Time.FromUnixTime(details.Start + details.Duration + details.Review);
        }

        public RoundInfo Info { get; set; }
        public RoundDetails Details { get; set; }

        // TODO: Handle transitions (get round (winners) when round ends)
        public string status
        {
            get
            {
                if (DateTime.Now < startDate)
                {
                    return STATE_SCHEDULED;
                }
                else if (DateTime.Now < endDate)
                {
                    if (Bounty == 0)
                    {
                        return STATE_UNFUNDED;
                    }
                    return STATE_OPEN;
                }
                else if (DateTime.Now < reviewEndDate)
                {
                    if (closed)
                    {
                        return STATE_CLOSED;
                    }
                    else if (totalSubmissions == 0)
                    {
                        return STATE_ABANDONED;
                    }
                    else if (winningSubmissions.Count > 0)
                    {
                        return STATE_HASWINNERS;
                    }

                    return STATE_INREVIEW;
                }
                else if (winningSubmissions.Count > 0)
                {
                    return STATE_CLOSED;
                }

                return STATE_ABANDONED;
            }
        }

        public DateTime startDate;
        public DateTime endDate;
        public DateTime reviewEndDate;

        public static readonly string STATE_SCHEDULED = "scheduled";
        public static readonly string STATE_UNFUNDED = "notFunded";
        public static readonly string STATE_OPEN = "open";
        public static readonly string STATE_INREVIEW = "review";
        public static readonly string STATE_HASWINNERS = "hasWinners";
        public static readonly string STATE_CLOSED = "closed";
        public static readonly string STATE_ABANDONED = "abandoned";

        Dictionary<string, string> displayStates = new Dictionary<string, string>() { {STATE_SCHEDULED, "Scheduled" }, {STATE_UNFUNDED, "Unfunded" }, {STATE_OPEN, "Open" }, {STATE_INREVIEW, "In Review" }, {STATE_HASWINNERS, "Has Winners" }, { STATE_CLOSED, "Closed" }, {STATE_ABANDONED, "Abandoned" } };

        public string StatusText
        {
            get
            {
                TimeSpan? timeRemaining = null;
                string sts = status;
                string displayState = displayStates[status];
                if (sts.Equals(STATE_OPEN))
                {
                    timeRemaining = endDate - DateTime.Now;
                    displayState = "";
                }
                else if(sts.Equals(STATE_INREVIEW) || sts.Equals(STATE_HASWINNERS))
                {
                    timeRemaining = reviewEndDate - DateTime.Now;
                    displayState = " ( " + displayState + " )";
                }

                if (timeRemaining.HasValue)
                {
                    var unitLabels = new string[] { "Seconds", "Minutes", "Hours", " Days", "Weeks", "Years" };
                    var amountPerNextUnit = new double[] { 60, 60, 24, 7, 54 };

                    var amount = timeRemaining.Value.TotalSeconds;
                    int i = 0;
                    for (; i < unitLabels.Length; i++)
                    {
                        if (amount < amountPerNextUnit[i]) break;
                        amount = amount / amountPerNextUnit[i];
                    }

                    var time = Math.Truncate(amount) + unitLabels[i];
                    return time + displayState;
                }
                else
                {
                    return displayState;
                }
            }
        }

        public int index;
        public bool closed;
        public int Number { get { return index + 1; } }
        public int totalParticipants;
        public int totalSubmissions;
        public List<MatryxSubmission> allSubmissions = new List<MatryxSubmission>();
        public List<MatryxSubmission> winningSubmissions = new List<MatryxSubmission>();
        public decimal Bounty
        {
            get
            {
                return (decimal)(Details.Bounty / new BigInteger((decimal)1e18));
            }
            set
            {
                if (Details == null)
                {
                    Details = new RoundDetails();
                }

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
