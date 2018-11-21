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

using Matryx;
using Nanome.Core;

namespace Matryx
{
    public class MatryxTournament
    {
        public string address;
        public static string ABI;
        Nethereum.Contracts.Contract contract;

        public MatryxTournament(string address)
        {
            this.address = address;
        }
        public MatryxTournament(string address, string title, long bounty)
        {
            this.address = address;
            this.title = title;
            this.bounty = bounty;
        }

        public string owner;
        public string title;
        public string descriptionHash;
        public string fileHash;
        public string category;
        public long bounty;
        public int currentRound;
        public string currentRoundAddress;
        public string currentRoundState;
        public long roundEndTime;
        public int numberOfParticipants;
        public int entryFee;

        public string description;
        public byte[] file;

        public string getDescription()
        {
            if (description == null)
            {
                // ipfs call
            }

            return description;
        }

        public byte[] getFile()
        {
            if (file == null)
            {
                //ipfs call
            }

            return file;
        }

        [Function("getRounds", "address[]")]
        public class GetRoundsFunction : FunctionMessage { }
        [Function("getCurrentRound", typeof(CurrentRoundOutputDTO))]
        public class GetCurrentRoundFunction : FunctionMessage { }
        [FunctionOutput]
        public class CurrentRoundOutputDTO : IFunctionOutputDTO
        {
            [Parameter("uint256")]
            public uint RoundNumber { get; set; }
            [Parameter("address")]
            public string RoundAddress { get; set; }
        }
        [Function("getSubmissionCount", "uint256")]
        public class GetSubmissionCountFunction : FunctionMessage { }
        [Function("isEntrant", "bool")]
        public class IsEntrantFunction : FunctionMessage
        {
            [Parameter("address")]
            public string User { get; set; }
        }

        [Function("enter")]
        public class EnterFunction : FunctionMessage { }
        [Function("exit")]
        public class ExitFunction : FunctionMessage { }

        [Function("createSubmission")]
        public class CreateSubmissionFunction : FunctionMessage
        {
            [Parameter("tuple", "SubmissionDetails")]
            public SubmissionDetails Details { get; set; }
        }

        [FunctionOutput]
        public class SubmissionDetails : IFunctionOutputDTO
        {
            [Parameter("bytes32[3]")]
            public List<string> Title { get; set; }
            [Parameter("bytes32[2]")]
            public List<string> DescHash { get; set; }
            [Parameter("bytes32[2]")]
            public List<string> FileHash { get; set; }
            [Parameter("uint256[]")]
            public List<BigInteger> Distribution { get; set; }
            [Parameter("address[]")]
            public List<string> Contributors { get; set; }
            [Parameter("address[]")]
            public List<string> References { get; set; }
        }

        [Function("selectWinners")]
        public class SelectWinnersFunction : FunctionMessage
        {
            [Parameter("tuple", "WinnersData")]
            public WinnersData WData { get; set; }
            [Parameter("tuple", "RoundDetails")]
            public MatryxRound.RoundDetails RDetails { get; set; }
        }

        [FunctionOutput]
        public class WinnersData : IFunctionOutputDTO
        {
            [Parameter("address[]")]
            public List<string> Submissions { get; set; }
            [Parameter("uint256[]")]
            public List<BigInteger> Distribution { get; set; }
            [Parameter("uint256")]
            public BigInteger Action { get; set; }
        }

        [Function("startNextRound")]
        public class StartNextRoundFunction : FunctionMessage {}

        [Function("closeTournament")]
        public class CloseTournamentFunction : FunctionMessage { }

        [Function("voteSubmission")]
        public class VoteSubmissionFunction : FunctionMessage
        {
            [Parameter("address")]
            public string Submission { get; set; }
            [Parameter("bool")]
            public bool Vote { get; set; }
        }

        [Function("voteRound")]
        public class VoteRoundFunction : FunctionMessage
        {
            [Parameter("address")]
            public string Round { get; set; }
            [Parameter("bool")]
            public bool Vote { get; set; }
        }

        [Function("withdrawFromAbandoned")]
        public class WithdrawFromAbandonedFunction : FunctionMessage {}

        [Function("recoverFunds")]
        public class RecoverFundsFunction : FunctionMessage {}

        public IEnumerator getRounds()
        {
            var getRoundsRequest = new QueryUnityRequest<GetRoundsFunction, EthereumTypes.AddressArray>(Network.infuraProvider, Network.account);
            yield return getRoundsRequest.Query(new GetRoundsFunction(), address);
        }

        public IEnumerator getCurrentRound()
        {
            var getCurrentRoundRequest = new QueryUnityRequest<GetCurrentRoundFunction, CurrentRoundOutputDTO>(Network.infuraProvider, Network.account);
            yield return getCurrentRoundRequest.Query(new GetCurrentRoundFunction(), address);
        }

        public IEnumerator getSubmissionCount()
        {
            var getSubmissionCountRequest = new QueryUnityRequest<GetSubmissionCountFunction, EthereumTypes.Uint256>(Network.infuraProvider, Network.account);
            yield return getSubmissionCountRequest.Query(new GetSubmissionCountFunction(), address);
        }

        public IEnumerator isEntrant(string user)
        {
            var isEntrantRequest = new QueryUnityRequest<IsEntrantFunction, EthereumTypes.Bool>(Network.infuraProvider, Network.account);
            yield return isEntrantRequest.Query(new IsEntrantFunction() { User = user }, address);
            Debug.Log(isEntrantRequest.Result.Value);
        }

        public IEnumerator enter()
        {
            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            yield return transactionRequest.SignAndSendTransaction<EnterFunction>(new EnterFunction() { Gas = Network.txGas, GasPrice = Network.txGasPrice }, address);

            yield return Utils.HandleSignedSentTransaction(transactionRequest);
        }

        public IEnumerator exit()
        {
            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            yield return transactionRequest.SignAndSendTransaction<ExitFunction>(new ExitFunction() { Gas = Network.txGas, GasPrice = Network.txGasPrice }, address);

            yield return Utils.HandleSignedSentTransaction(transactionRequest);
        }

        public IEnumerator createSubmission(Async thread, string title, string descHash, string fileHash, List<BigInteger> distribution, List<string> contributors, List<string> references)
        {
            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            yield return transactionRequest.SignAndSendTransaction<CreateSubmissionFunction>(new CreateSubmissionFunction() { Details = new SubmissionDetails() { Title = Utils.stringToBytes(title, 3), DescHash = Utils.stringToBytes(descHash, 2), FileHash = Utils.stringToBytes(fileHash, 2), Distribution = distribution, Contributors = contributors, References = references }, Gas = Network.txGas, GasPrice = Network.txGasPrice }, address);

            yield return Utils.HandleSignedSentTransaction(transactionRequest);
        }

        public IEnumerator selectWinners(Async thread, List<string> submissions, List<BigInteger> distribution, BigInteger action, BigInteger start, BigInteger end, BigInteger review, BigInteger bounty)
        {
            WinnersData wData = new WinnersData() { Submissions = submissions, Distribution = distribution, Action = action };
            MatryxRound.RoundDetails rDetails = new MatryxRound.RoundDetails() { Start = start, End = end, Review = review, Bounty = bounty };

            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            yield return transactionRequest.SignAndSendTransaction<SelectWinnersFunction>(new SelectWinnersFunction() { WData = wData, RDetails = rDetails, Gas = Network.txGas, GasPrice = Network.txGasPrice }, address);

            yield return Utils.HandleSignedSentTransaction(transactionRequest);
        }

        public IEnumerator startNextRound(Async thread)
        {
            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            yield return transactionRequest.SignAndSendTransaction<StartNextRoundFunction>(new StartNextRoundFunction() { Gas = Network.txGas, GasPrice = Network.txGasPrice }, address);

            yield return Utils.HandleSignedSentTransaction(transactionRequest);
        }

        public IEnumerator closeTournament(Async thread)
        {
            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            yield return transactionRequest.SignAndSendTransaction<CloseTournamentFunction>(new CloseTournamentFunction() { Gas = Network.txGas, GasPrice = Network.txGasPrice }, address);

            yield return Utils.HandleSignedSentTransaction(transactionRequest);
        }

        public IEnumerator voteSubmission(Async thread, string submission, bool vote)
        {
            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            yield return transactionRequest.SignAndSendTransaction<VoteSubmissionFunction>(new VoteSubmissionFunction() { Submission = submission, Vote = vote, Gas = Network.txGas, GasPrice = Network.txGasPrice }, address);

            yield return Utils.HandleSignedSentTransaction(transactionRequest);
        }

        public IEnumerator voteRound(Async thread, string round, bool vote)
        {
            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            yield return transactionRequest.SignAndSendTransaction<VoteRoundFunction>(new VoteRoundFunction() { Round = round, Vote = vote, Gas = Network.txGas, GasPrice = Network.txGasPrice }, address);

            yield return Utils.HandleSignedSentTransaction(transactionRequest);
        }

        public IEnumerator withdrawFromAbandoned(Async thread)
        {
            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            yield return transactionRequest.SignAndSendTransaction<WithdrawFromAbandonedFunction>(new WithdrawFromAbandonedFunction() { Gas = Network.txGas, GasPrice = Network.txGasPrice }, address);

            yield return Utils.HandleSignedSentTransaction(transactionRequest);
        }

        public IEnumerator recoverFunds()
        {
            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            yield return transactionRequest.SignAndSendTransaction<RecoverFundsFunction>(new RecoverFundsFunction() { Gas = Network.txGas, GasPrice = Network.txGasPrice }, address);

            yield return Utils.HandleSignedSentTransaction(transactionRequest);
        }
    }
}