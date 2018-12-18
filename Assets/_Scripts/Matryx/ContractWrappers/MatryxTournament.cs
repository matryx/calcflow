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

        public string owner;
        public string title;
        public string descHash = string.Empty;
        public string fileHash = string.Empty;
        public string category;
        public BigInteger bounty;
        public BigInteger Bounty {
            get
            {
                return (bounty / new BigInteger(1e18));
            }
            set
            {
                bounty = value * new BigInteger(1e18);
            }
        }
        public BigInteger entryFee;
        public BigInteger EntryFee
        {
            get
            {
                return (entryFee / new BigInteger(1e18));
            }
            set
            {
                entryFee = value * new BigInteger(1e18);
            }
        }
        public int currentRound;
        public string currentRoundAddress;
        public string currentRoundState;
        public long roundEndTime;
        public int numberOfParticipants;

        public List<MatryxRound> rounds;

        public string description = "";
        public string file = "";

        public MatryxTournament(string address)
        {
            this.address = address;
        }
        public MatryxTournament(string address, string title, BigInteger bounty, BigInteger entryFee)
        {
            this.address = address;
            this.title = title;
            this.bounty = bounty;
            this.entryFee = entryFee;
        }
        public MatryxTournament(string title, string description, string file, string category, BigInteger bounty, BigInteger entryFee, MatryxRound.RoundDetails roundDetails)
        {
            this.title = title;
            this.description = description;
            this.file = file;
            this.category = category;
            this.bounty = bounty;
            this.entryFee = entryFee;

            rounds = new List<MatryxRound>();
            rounds.Add(new MatryxRound());
            rounds[0].Details = roundDetails;
        }

        public string getDescription()
        {
            if (description == null)
            {
                // ipfs call
            }

            return description;
        }

        public string getFile()
        {
            if (file == null)
            {
                //ipfs call
            }

            return file;
        }

        [Function("getOwner", "address")]
        public class GetOwnerFunction : FunctionMessage { }

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

        public IEnumerator create(Async.EventDelegate callback = null)
        {
            ResultsMenu.transactionObject = this;

            var allowance = new Utils.CoroutineWithData<BigInteger>(MatryxExplorer.Instance, MatryxToken.allowance(NetworkSettings.address, MatryxPlatform.address));
            yield return allowance;

            if (allowance.result < bounty)
            {
                ResultsMenu.Instance.SetStatus("Approving MatryxPlatform for "  + bounty + " MTX...");

                if (allowance.result != BigInteger.Zero)
                {
                    var approveZero = new Utils.CoroutineWithData<bool>(MatryxExplorer.Instance, MatryxToken.approve(MatryxPlatform.address, BigInteger.Zero));
                    yield return approveZero;

                    if (!approveZero.result)
                    {
                        Debug.Log("Failed to reset tournament's allowance to zero for this user. Please check the allowance this user has granted the tournament");
                        yield break;
                    }
                }

                var approveBounty = new Utils.CoroutineWithData<bool>(MatryxExplorer.Instance, MatryxToken.approve(MatryxPlatform.address, bounty));
                yield return approveBounty;

                if (!approveBounty.result)
                {
                    Debug.Log("Failed to set the tournament's allowance from this user to the tournament's entry fee");
                    yield break;
                }
            }

            if (!description.Equals(string.Empty) && descHash.Equals(string.Empty))
            {
                ResultsMenu.Instance.SetStatus("Uploading content to IPFS...");
                var uploadToIPFS = new Utils.CoroutineWithData<string[]>(MatryxExplorer.Instance, Utils.uploadToIPFS(this));
                yield return uploadToIPFS;

                if(!uploadToIPFS.result[0].Equals(string.Empty))
                {
                    descHash = uploadToIPFS.result[0];
                }
                if(!uploadToIPFS.result[1].Equals(string.Empty))
                {
                    fileHash = uploadToIPFS.result[1];
                }
            }

            ResultsMenu.Instance.SetStatus("Creating Tournament " + title + "...");
            var createTournament = new Utils.CoroutineWithData<bool>(MatryxExplorer.Instance, MatryxPlatform.createTournament(this));
            yield return createTournament;

            if (callback != null)
            {
                callback(createTournament.result);
            }
        }

        public IEnumerator getOwner()
        {
            var getOwnerRequest = new QueryUnityRequest<GetOwnerFunction, EthereumTypes.Address>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return getOwnerRequest.Query(new GetOwnerFunction(), address);
            owner = getOwnerRequest.Result.Value;
            yield return getOwnerRequest.Result.Value;
        }

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