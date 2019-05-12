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
using Calcflow.UserStatistics;
using System;

namespace Matryx
{
    public class MatryxTournament
    {
        public enum SelectWinnerAction
        {
            DoNothing = 0,
            StartNextRound,
            CloseTournament
        }

        public string address;
        public static string ABI;
        Nethereum.Contracts.Contract contract;

        public string owner;
        public string contentHash = "";
        public string title = "";
        public string description = "";
        public string file = "";
        public string fileHash = "";
        public string category = "";
        public string latestRoundState = "";
        public BigInteger bounty;
        public BigInteger Bounty {
            get
            {
                return (bounty / new BigInteger((decimal)1e18));
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
        public MatryxRound currentRound;
        public int numberOfParticipants;

        public List<MatryxRound> rounds;

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

        public string getFile()
        {
            if (file == null)
            {
                //ipfs call
            }

            return file;
        }

        //function getInfo() external view returns(LibTournament.TournamentInfo memory);
        [Function("getInfo", typeof(TournamentInfo))]
        public class GetInfoFunction : FunctionMessage { }
        [FunctionOutput]
        public class TournamentInfo : IFunctionOutputDTO
        {
            [Parameter("uint256")]
            public BigInteger version { get; set; }
            [Parameter("address")]
            public string owner { get; set; }
        }


        //function getDetails() external view returns(LibTournament.TournamentDetails memory);
        [Function("getDetails", typeof(TournamentDetails))]
        public class GetDetailsFunction : FunctionMessage { }
        [FunctionOutput]
        public class TournamentDetails : IFunctionOutputDTO
        {
            [Parameter("string")]
            public string content { get; set; }
            [Parameter("uint256")]
            public BigInteger bounty { get; set; }
            [Parameter("uint256")]
            public BigInteger entryFee { get; set; }
        }

        //function getBalance() external view returns(uint256);
        [Function("getBalance", "uint256")]
        public class GetBalanceFunction : FunctionMessage { }

        //function getState() external view returns(uint256);
        [Function("getState", "uint256")]
        public class GetStateFunction : FunctionMessage { }

        //function getRoundState(uint256 roundIndex) external view returns(uint256);
        [Function("getRoundState", "uint256")]
        public class GetRoundStateFunction : FunctionMessage
        {
            [Parameter("uint256")]
            public BigInteger RoundIndex { get; set; }
        }

        //function getCurrentRoundIndex() external view returns(uint256);
        [Function("getCurrentRoundIndex", "uint256")]
        public class GetCurrentRoundIndexFunction : FunctionMessage { }

        //function getRoundInfo(uint256 roundIndex) external view returns(LibTournament.RoundInfo memory);
        [Function("getRoundInfo", typeof(MatryxRound.RoundInfo))]
        public class GetRoundInfoFunction : FunctionMessage
        {
            [Parameter("uint256")]
            public BigInteger RoundIndex { get; set; }
        }

        //function getRoundDetails(uint256 roundIndex) external view returns(LibTournament.RoundDetails memory);
        [Function("getRoundDetails", typeof(MatryxRound.RoundDetails))]
        public class GetRoundDetailsFunction : FunctionMessage
        {
            [Parameter("uint256")]
            public BigInteger RoundIndex { get; set; }
        }

        //function getSubmissionCount() external view returns(uint256);
        [Function("getSubmissionCount", "uint256")]
        public class GetSubmissionCountFunction : FunctionMessage { }

        //function getEntryFeePaid(address user) external view returns(uint256);
        [Function("getEntryFeePaid", "uint256")]
        public class GetEntryFeePaidFunction : FunctionMessage
        {
            [Parameter("address")]
            public string User { get; set; }
        }

        //function isEntrant(address user) external view returns(bool);
        [Function("isEntrant", "bool")]
        public class IsEntrantFunction : FunctionMessage
        {
            [Parameter("address")]
            public string User { get; set; }
        }

        //function enter() external;
        [Function("enter")]
        public class EnterFunction : FunctionMessage { }

        //function exit() external;
        [Function("exit")]
        public class ExitFunction : FunctionMessage { }

        //function createSubmission(string calldata content, bytes32 commitHash) external;
        [Function("createSubmission")]
        public class CreateSubmissionFunction : FunctionMessage
        {
            [Parameter("string")]
            public string Content { get; set; }
            [Parameter("bytes32")]
            public byte[] CommitHash { get; set; }
        }

        //function updateDetails(LibTournament.TournamentDetails calldata tournamentDetails) external;
        [Function("updateDetails")]
        public class UpdateDetailsFunction : FunctionMessage
        {
            [Parameter("tuple")]
            public TournamentDetails TDetails { get; set; }
        }

        //function addToBounty(uint256 amount) external;
        [Function("addToBounty")]
        public class AddToBountyFunction : FunctionMessage
        {
            [Parameter("uint256")]
            public BigInteger Amount { get; set; }
        }

        //function transferToRound(uint256 amount) external;
        [Function("transferToRound")]
        public class TransferToRoundFunction : FunctionMessage
        {
            [Parameter("uint256")]
            public BigInteger Amount { get; set; }
        }

        //function selectWinners(LibTournament.WinnersData calldata winnersData, LibTournament.RoundDetails calldata roundDetails) external;
        [Function("selectWinners")]
        public class SelectWinnersFunction : FunctionMessage
        {
            [Parameter("tuple")]
            public WinnersData WData { get; set; }
            [Parameter("tuple")]
            public MatryxRound.RoundDetails RDetails { get; set; }
        }

        [FunctionOutput]
        public class WinnersData : IFunctionOutputDTO
        {
            [Parameter("bytes32[]")]
            public List<byte[]> Submissions { get; set; }
            [Parameter("uint256[]")]
            public List<BigInteger> Distribution { get; set; }
            [Parameter("uint256")]
            public BigInteger Action { get; set; }
        }

        //function updateNextRound(LibTournament.RoundDetails calldata roundDetails) external;
        [Function("updateNextRound")]
        public class UpdateNextRoundFunction : FunctionMessage
        {
            [Parameter("tuple")]
            public MatryxRound.RoundDetails RDetails { get; set; }
        }

        //function startNextRound() external;
        [Function("startNextRound")]
        public class StartNextRoundFunction : FunctionMessage { }

        //function closeTournament() external;
        [Function("closeTournament")]
        public class CloseTournamentFunction : FunctionMessage { }

        //function withdrawFromAbandoned() external;
        [Function("withdrawFromAbandoned")]
        public class WithdrawFromAbandonedFunction : FunctionMessage { }

        //function recoverBounty() external;
        [Function("recoverBounty")]
        public class RecoverBountyFunction : FunctionMessage { }

        /// <summary>
        /// Uploads a Tournament's description and files to IPFS.
        /// </summary>
        /// <param name="tournament"> The tournament whose description and files are to be uploaded. </param>
        /// <returns> The description hash and files hash of the tournament in that order. </returns>
        public IEnumerator uploadContent()
        {
            string contentHash = "";
            string filesHash = "";

            // TODO: Allow for file uploading for tournaments (FileBrowser)
            //if (tournament.fileHash == null || tournament.fileHash.Equals(string.Empty))
            //{
            //    if (tournament.file != null && !tournament.file.Equals(string.Empty))
            //    {
            //        var uploadToIPFS = new Utils.CoroutineWithData<string>(MatryxCortex.Instance, Utils.uploadFiles("filesContent", "", tournament.file, "text/plain"));
            //        yield return uploadToIPFS;
            //        filesHash = uploadToIPFS.result;
            //    }
            //}

            if (contentHash == string.Empty)
            {
                if (description != null && !description.Equals(string.Empty))
                {
                    var uploadToIPFS = new Utils.CoroutineWithData<string>(MatryxCortex.Instance, MatryxCortex.uploadJson(title, description, "", "math"));
                    yield return uploadToIPFS;
                    contentHash = uploadToIPFS.result;
                }
            }

            yield return new string[2] { contentHash, filesHash };
        }

        //function createSubmission(string calldata content, bytes32 commitHash) external;
        public IEnumerator create(Async.EventDelegate callback = null)
        {
            StatisticsTracking.StartEvent("Matryx", "Tournament Creation");

            ResultsMenu.transactionObject = this;
            ResultsMenu.Instance.SetStatus("Checking MTX balance and platform allowance...");

            var allowance = new Utils.CoroutineWithData<BigInteger>(MatryxCortex.Instance, MatryxToken.allowance(NetworkSettings.currentAddress, MatryxPlatform.address));
            yield return allowance;

            var balance = new Utils.CoroutineWithData<BigInteger>(MatryxCortex.Instance, MatryxToken.balanceOf(NetworkSettings.currentAddress));
            yield return balance;

            if (balance.result < bounty)
            {
                ResultsMenu.Instance.SetStatus("Insufficient MTX. Please visit <link=https://app.matryx.ai/><u>our Matryx Dapp</u></link> for MTX Tokens.", true);
                ResultsMenu.Instance.ReturnToCalcflowAfterSeconds(8f);
                yield break;
            }

            if (allowance.result < bounty)
            {
                ResultsMenu.Instance.SetStatus("Approving MatryxPlatform for " + Bounty + " MTX...");

                if (allowance.result != BigInteger.Zero)
                {
                    var approveZero = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, MatryxToken.approve(MatryxPlatform.address, BigInteger.Zero));
                    yield return approveZero;

                    if (!approveZero.result)
                    {
                        ResultsMenu.Instance.PostFailure(this, "Failed to reset the platform allowance to zero");
                        yield break;
                    }
                }

                var approveBounty = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, MatryxToken.approve(MatryxPlatform.address, bounty));
                yield return approveBounty;

                if (!approveBounty.result)
                {
                    ResultsMenu.Instance.PostFailure(this, "Failed to give the platform an MTX allowance");
                    yield break;
                }
            }

            if (contentHash.Equals(""))
            {
                ResultsMenu.Instance.SetStatus("Uploading Tournament Content...");
                var uploadToIPFS = new Utils.CoroutineWithData<string[]>(MatryxCortex.Instance, uploadContent());
                yield return uploadToIPFS;

                if (!uploadToIPFS.result[0].Equals(string.Empty))
                {
                    contentHash = uploadToIPFS.result[0];
                }
                if (!uploadToIPFS.result[1].Equals(string.Empty))
                {
                    fileHash = uploadToIPFS.result[1];
                }
            }

            ResultsMenu.Instance.SetStatus("Creating Tournament " + title + "...");
            var createTournament = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, MatryxPlatform.createTournament(this));
            yield return createTournament;

            callback?.Invoke(createTournament.result);
        }

        public IEnumerator getInfo()
        {
            var request = new QueryUnityRequest<GetInfoFunction, TournamentInfo>(NetworkSettings.infuraProvider, NetworkSettings.currentAddress);
            yield return request.Query(new GetInfoFunction(), address);
            owner = request.Result.owner.ToLower();
            yield return request.Result;
        }

        public IEnumerator getDetails()
        {
            var request = new QueryUnityRequest<GetDetailsFunction, TournamentDetails>(NetworkSettings.infuraProvider, NetworkSettings.currentAddress);
            yield return request.Query(new GetDetailsFunction(), address);
        }

        public IEnumerator getBalance()
        {
            var request = new QueryUnityRequest<GetBalanceFunction, EthereumTypes.Uint256>(NetworkSettings.infuraProvider, NetworkSettings.currentAddress);
            yield return request.Query(new GetBalanceFunction(), address);
            yield return request.Result;
        }

        public IEnumerator getState()
        {
            var request = new QueryUnityRequest<GetStateFunction, EthereumTypes.Uint256>(NetworkSettings.infuraProvider, NetworkSettings.currentAddress);
            yield return request.Query(new GetStateFunction(), address);
            yield return request.Result;
        }

        public IEnumerator getRoundState(BigInteger roundIndex)
        {
            var request = new QueryUnityRequest<GetRoundStateFunction, EthereumTypes.Uint256>(NetworkSettings.infuraProvider, NetworkSettings.currentAddress);
            yield return request.Query(new GetRoundStateFunction() { RoundIndex = roundIndex }, address);
            yield return request.Result;
        }

        public IEnumerator getCurrentRoundIndex()
        {
            var request = new QueryUnityRequest<GetCurrentRoundIndexFunction, EthereumTypes.Uint256>(NetworkSettings.infuraProvider, NetworkSettings.currentAddress);
            yield return request.Query(new GetCurrentRoundIndexFunction(), address);
            yield return request.Result;
        }

        public IEnumerator getRoundInfo(BigInteger roundIndex)
        {
            var request = new QueryUnityRequest<GetRoundInfoFunction, MatryxRound.RoundInfo>(NetworkSettings.infuraProvider, NetworkSettings.currentAddress);
            yield return request.Query(new GetRoundInfoFunction() { RoundIndex = roundIndex }, address);
        }

        public IEnumerator getRoundDetails(BigInteger roundIndex)
        {
            var request = new QueryUnityRequest<GetRoundDetailsFunction, MatryxRound.RoundDetails>(NetworkSettings.infuraProvider, NetworkSettings.currentAddress);
            yield return request.Query(new GetRoundDetailsFunction() { RoundIndex = roundIndex }, address);
        }

        public IEnumerator getSubmissionCount()
        {
            var request = new QueryUnityRequest<GetSubmissionCountFunction, EthereumTypes.Uint256>(NetworkSettings.infuraProvider, NetworkSettings.currentAddress);
            yield return request.Query(new GetSubmissionCountFunction(), address);
            yield return request.Result;
        }

        public IEnumerator getEntryFeePaid(string user)
        {
            var request = new QueryUnityRequest<GetEntryFeePaidFunction, EthereumTypes.Address>(NetworkSettings.infuraProvider, NetworkSettings.currentAddress);
            yield return request.Query(new GetEntryFeePaidFunction() { User = user }, address);
            yield return request.Result;
        }

        public IEnumerator isEntrant(string user)
        {
            var isEntrantRequest = new QueryUnityRequest<IsEntrantFunction, EthereumTypes.Bool>(NetworkSettings.infuraProvider, NetworkSettings.currentAddress);
            yield return isEntrantRequest.Query(new IsEntrantFunction() { User = user }, address);
            yield return isEntrantRequest.Result;
        }

        public IEnumerator enter(Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.currentPrivateKey);
            yield return transactionRequest.SignAndSendTransaction<EnterFunction>(new EnterFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            var txStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "enter", thread));
            yield return txStatus;
            yield return txStatus.result;
        }

        public IEnumerator exit(Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.currentPrivateKey);
            yield return transactionRequest.SignAndSendTransaction<ExitFunction>(new ExitFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            yield return Utils.GetTransactionStatus(transactionRequest, "exit", thread);
        }

        public IEnumerator createSubmission(MatryxSubmission submission, Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.currentPrivateKey);
            var createSubmissionMessage = new CreateSubmissionFunction()
            {
                Content = submission.dto.Content,
                CommitHash = Utils.HexStringToByteArray(submission.commit.hash),
                Gas = NetworkSettings.txGas,
                GasPrice = NetworkSettings.txGasPrice
            };
            yield return transactionRequest.SignAndSendTransaction<CreateSubmissionFunction>(createSubmissionMessage, address);

            var getTransactionStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "createSubmission", thread));
            yield return getTransactionStatus;
            yield return getTransactionStatus.result;
        }

        public IEnumerator updateDetails(TournamentDetails newDetails, Async thread = null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.currentPrivateKey);
            yield return transactionRequest.SignAndSendTransaction<UpdateDetailsFunction>(new UpdateDetailsFunction() { TDetails = newDetails, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            var getTransactionStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "createSubmission", thread));
            yield return getTransactionStatus;
            yield return getTransactionStatus.result;
        }

        public IEnumerator addToBounty(BigInteger amount, Async.EventDelegate onComplete = null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.currentPrivateKey);
            yield return transactionRequest.SignAndSendTransaction<AddToBountyFunction>(new AddToBountyFunction() { Amount = amount, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            var getTransactionStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "addToBounty", null));
            yield return getTransactionStatus;
            yield return getTransactionStatus.result;

            onComplete?.Invoke(getTransactionStatus.result);
        }

        public IEnumerator transferToRound(BigInteger amount, Async thread = null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.currentPrivateKey);
            yield return transactionRequest.SignAndSendTransaction<TransferToRoundFunction>(new TransferToRoundFunction() { Amount = amount, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            var getTransactionStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "transferToRound", thread));
            yield return getTransactionStatus;
            yield return getTransactionStatus.result;
        }

        public IEnumerator selectWinners(List<byte[]> submissions, List<BigInteger> distribution, BigInteger action, BigInteger start, BigInteger duration, BigInteger review, BigInteger bounty, Async.EventDelegate onComplete=null)
        {
            WinnersData wData = new WinnersData() { Submissions = submissions, Distribution = distribution, Action = action };
            MatryxRound.RoundDetails rDetails = new MatryxRound.RoundDetails() { Start = start, Duration = duration, Review = review, Bounty = bounty };

            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.currentPrivateKey);
            yield return transactionRequest.SignAndSendTransaction<SelectWinnersFunction>(new SelectWinnersFunction() { WData = wData, RDetails = rDetails, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            yield return Utils.GetTransactionStatus(transactionRequest, "selectWinners", null);
            

            var getTransactionStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "selectWinners", null));
            yield return getTransactionStatus;
            yield return getTransactionStatus.result;

            onComplete?.Invoke(getTransactionStatus.result);
        }

        public IEnumerator updateNextRound(MatryxRound.RoundDetails newDetails, Async.EventDelegate onComplete = null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.currentPrivateKey);
            yield return transactionRequest.SignAndSendTransaction<UpdateNextRoundFunction>(new UpdateNextRoundFunction() { RDetails = newDetails, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            var getTransactionStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "updateNextRound", null));
            yield return getTransactionStatus;
            yield return getTransactionStatus.result;
            onComplete?.Invoke(getTransactionStatus.result);
        }

        public IEnumerator startNextRound(Async.EventDelegate onComplete=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.currentPrivateKey);
            yield return transactionRequest.SignAndSendTransaction<StartNextRoundFunction>(new StartNextRoundFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            var getTransactionStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "startNextRound", null));
            yield return getTransactionStatus;
            yield return getTransactionStatus.result;
            onComplete?.Invoke(getTransactionStatus.result);
        }

        public IEnumerator closeTournament(Async.EventDelegate onComplete = null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.currentPrivateKey);
            yield return transactionRequest.SignAndSendTransaction<CloseTournamentFunction>(new CloseTournamentFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            var getTransactionStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "closeTournament", null));
            yield return getTransactionStatus;
            yield return getTransactionStatus.result;
            onComplete?.Invoke(getTransactionStatus.result);
        }

        public IEnumerator withdrawFromAbandoned(Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.currentPrivateKey);
            yield return transactionRequest.SignAndSendTransaction<WithdrawFromAbandonedFunction>(new WithdrawFromAbandonedFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            yield return Utils.GetTransactionStatus(transactionRequest, "withdrawFromAbandoned", thread);
        }

        public IEnumerator recoverBounty(Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.currentPrivateKey);
            yield return transactionRequest.SignAndSendTransaction<RecoverBountyFunction>(new RecoverBountyFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice }, address);

            yield return Utils.GetTransactionStatus(transactionRequest, "recoverFunds", thread);
        }
    }
}