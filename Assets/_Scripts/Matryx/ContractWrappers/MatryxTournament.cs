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
        public MatryxTournament(string address, string title, BigInteger bounty)
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
        public BigInteger bounty;
        public int currentRound;
        public string currentRoundAddress;
        public string currentRoundState;
        public long roundEndTime;
        public int numberOfParticipants;
        public BigInteger entryFee;

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
        [Function("getEntryFee", "uint256")]
        public class GetEntryFeeFunction : FunctionMessage { }

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

        public IEnumerator getEntryFee()
        {
            var getEntryFeeRequest = new QueryUnityRequest<GetEntryFeeFunction, EthereumTypes.Uint256>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return getEntryFeeRequest.Query(new GetEntryFeeFunction(), address);
            entryFee = getEntryFeeRequest.Result.Value;
        }

        public IEnumerator getRounds()
        {
            var getRoundsRequest = new QueryUnityRequest<GetRoundsFunction, EthereumTypes.AddressArray>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return getRoundsRequest.Query(new GetRoundsFunction(), address);
            yield return getRoundsRequest.Result.Value;
        }

        public IEnumerator getCurrentRound()
        {
            var getCurrentRoundRequest = new QueryUnityRequest<GetCurrentRoundFunction, CurrentRoundOutputDTO>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return getCurrentRoundRequest.Query(new GetCurrentRoundFunction(), address);
        }

        public IEnumerator getSubmissionCount()
        {
            var getSubmissionCountRequest = new QueryUnityRequest<GetSubmissionCountFunction, EthereumTypes.Uint256>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return getSubmissionCountRequest.Query(new GetSubmissionCountFunction(), address);
        }

        public IEnumerator isEntrant(string user)
        {
            var isEntrantRequest = new QueryUnityRequest<IsEntrantFunction, EthereumTypes.Bool>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return isEntrantRequest.Query(new IsEntrantFunction() { User = user }, address);
            yield return isEntrantRequest.Result.Value;
        }

        public IEnumerator enter(Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.privateKey, NetworkSettings.address);
            yield return transactionRequest.SignAndSendTransaction<EnterFunction>(new EnterFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            var txStatus = new Utils.CoroutineWithData<bool>(MatryxExplorer.Instance, Utils.GetTransactionStatus(transactionRequest, "enter", thread));
            yield return txStatus;
            yield return txStatus.result;
        }

        public IEnumerator exit(Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.privateKey, NetworkSettings.address);
            yield return transactionRequest.SignAndSendTransaction<ExitFunction>(new ExitFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            yield return Utils.GetTransactionStatus(transactionRequest, "exit", thread);
        }

        public IEnumerator createSubmission(MatryxSubmission submission, Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.privateKey, NetworkSettings.address);
            var submissionDetails = new SubmissionDetails()
            {
                Title = Utils.stringToString32(submission.details.title, 3),
                DescHash = Utils.stringToString32(submission.details.descHash, 2),
                FileHash = Utils.stringToString32(submission.details.fileHash, 2),
                Distribution = submission.details.distribution,
                Contributors = submission.details.contributors,
                References = submission.details.references
            };
            yield return transactionRequest.SignAndSendTransaction<CreateSubmissionFunction>(new CreateSubmissionFunction() { Details = submissionDetails, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            var getTransactionStatus = new Utils.CoroutineWithData<bool>(MatryxExplorer.Instance, Utils.GetTransactionStatus(transactionRequest, "createSubmission", thread));
            yield return getTransactionStatus;
            yield return getTransactionStatus.result;
        }

        public IEnumerator selectWinners(List<string> submissions, List<BigInteger> distribution, BigInteger action, BigInteger start, BigInteger end, BigInteger review, BigInteger bounty, Async thread=null)
        {
            WinnersData wData = new WinnersData() { Submissions = submissions, Distribution = distribution, Action = action };
            MatryxRound.RoundDetails rDetails = new MatryxRound.RoundDetails() { Start = start, End = end, Review = review, Bounty = bounty };

            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.privateKey, NetworkSettings.address);
            yield return transactionRequest.SignAndSendTransaction<SelectWinnersFunction>(new SelectWinnersFunction() { WData = wData, RDetails = rDetails, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            yield return Utils.GetTransactionStatus(transactionRequest, "selectWinners", thread);
        }

        public IEnumerator startNextRound(Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.privateKey, NetworkSettings.address);
            yield return transactionRequest.SignAndSendTransaction<StartNextRoundFunction>(new StartNextRoundFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            yield return Utils.GetTransactionStatus(transactionRequest, "startNextRound", thread);
        }

        public IEnumerator closeTournament(Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.privateKey, NetworkSettings.address);
            yield return transactionRequest.SignAndSendTransaction<CloseTournamentFunction>(new CloseTournamentFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            yield return Utils.GetTransactionStatus(transactionRequest, "closeTournament", thread);
        }

        public IEnumerator voteSubmission(string submission, bool vote, Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.privateKey, NetworkSettings.address);
            yield return transactionRequest.SignAndSendTransaction<VoteSubmissionFunction>(new VoteSubmissionFunction() { Submission = submission, Vote = vote, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            yield return Utils.GetTransactionStatus(transactionRequest, "voteSubmission", thread);
        }

        public IEnumerator voteRound(string round, bool vote, Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.privateKey, NetworkSettings.address);
            yield return transactionRequest.SignAndSendTransaction<VoteRoundFunction>(new VoteRoundFunction() { Round = round, Vote = vote, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            yield return Utils.GetTransactionStatus(transactionRequest, "voteRound", thread);
        }

        public IEnumerator withdrawFromAbandoned(Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.privateKey, NetworkSettings.address);
            yield return transactionRequest.SignAndSendTransaction<WithdrawFromAbandonedFunction>(new WithdrawFromAbandonedFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            yield return Utils.GetTransactionStatus(transactionRequest, "withdrawFromAbandoned", thread);
        }

        public IEnumerator recoverFunds(Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.privateKey, NetworkSettings.address);
            yield return transactionRequest.SignAndSendTransaction<RecoverFundsFunction>(new RecoverFundsFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            yield return Utils.GetTransactionStatus(transactionRequest, "recoverFunds", thread);
        }
    }
}