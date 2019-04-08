using System.Collections;
using System.Numerics;
using System.Collections.Generic;
using UnityEngine;
using Nethereum.Contracts;
using Nethereum.ABI.FunctionEncoding.Attributes;
using Nethereum.JsonRpc.UnityClient;
using Nanome.Core;

using BigInteger = System.Numerics.BigInteger;

namespace Matryx
{
    public static class MatryxPlatform
    {
        public static string address;
        public static string ABI;
        public static Nethereum.Contracts.Contract contract;

        public static void setContract(string abi, string addr)
        {
            address = addr;
            ABI = abi;
            contract = new Nethereum.Contracts.Contract(null, abi, addr);
        }

        //function getInfo() external view returns(MatryxPlatform.Info memory);
        [Function("getInfo", typeof(MatryxPlatformInfoDTO))]
        public class GetInfoFunction : FunctionMessage { }

        [FunctionOutput]
        public class MatryxPlatformInfoDTO : IFunctionOutputDTO
        {
            [Parameter("address")]
            public static string System { get; set; }
            [Parameter("address")]
            public static string Token { get; set; }
            [Parameter("address")]
            public static string Owner { get; set; }
        }

        //function isTournament(address tournament) external view returns(bool);
        [Function("isTournament", "address")]
        public class IsTournamentFunction : FunctionMessage
        {
            [Parameter("address")]
            public string TournamentAddress { get; set; }
        }

        //function isCommit(bytes32 commitHash) external view returns(bool);
        [Function("isCommit", "bool")]
        public class IsCommitFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public byte[] CommitHash { get; set; }
        }

        //function isSubmission(bytes32 submissionHash) external view returns(bool);
        [Function("isSubmission", "bool")]
        public class IsSubmissionFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public byte[] SubmissionHash { get; set; }
        }

        //function getTotalBalance() external view returns(uint256);
        [Function("getTotalBalance", "uint256")]
        public class GetTotalBalanceFunction : FunctionMessage { }

        //function getTournamentCount() external view returns(uint256);
        [Function("getTournamentCount", "uint256")]
        public class GetTournamentCountFunction : FunctionMessage { }

        //function getTournaments() external view returns(address[] memory);
        [Function("getTournaments", "address[]")]
        public class GetTournamentsFunction : FunctionMessage {}

        //function getSubmission(bytes32 submissionHash) external view returns(LibTournament.SubmissionData memory);
        [Function("getSubmission", typeof(MatryxSubmission.SubmissionDataDTO))]
        public class GetSubmissionFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public byte[] SubmissionHash { get; set; }
        }

        //function createTournament(LibTournament.TournamentDetails calldata, LibTournament.RoundDetails calldata) external returns(address);
        [Function("createTournament", "address")]
        public class CreateTournamentFunction : FunctionMessage
        {
            [Parameter("tuple", 1)]
            public TournamentDetails TDetails { get; set; }
            [Parameter("tuple", 11)]
            public MatryxRound.RoundDetails RDetails { get; set; }
        }

        [FunctionOutput]
        public class TournamentDetails : IFunctionOutputDTO
        {
            [Parameter("string")]
            public string ContentHash { get; set; }
            [Parameter("uint256")]
            public BigInteger Bounty { get; set; }
            [Parameter("uint256")]
            public BigInteger EntryFee { get; set; }
        }

        //function getInfo() external view returns(MatryxPlatform.Info);
        public static IEnumerator getInfo(Async thread=null)
        {
            var queryRequest = new QueryUnityRequest<GetInfoFunction, MatryxPlatformInfoDTO>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new GetInfoFunction(), address);
            yield return queryRequest.Result;

            if (thread != null)
            {
                thread.pushEvent("getInfo", queryRequest.Result);
            }
        }

        public static IEnumerator isTournament(string tournamentAddress, Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<IsTournamentFunction, EthereumTypes.Bool>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new IsTournamentFunction() { TournamentAddress = tournamentAddress }, address);
            yield return queryRequest.Result;

            if (thread != null)
            {
                thread.pushEvent("isTournament", queryRequest.Result);
            }
        }

        public static IEnumerator isCommit(string commitHash, Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<IsCommitFunction, EthereumTypes.Bool>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            var convertedHash = Utils.HexStringToByteArray(commitHash);
            yield return queryRequest.Query(new IsCommitFunction() { CommitHash = convertedHash }, address);
            yield return queryRequest.Result;

            if (thread != null)
            {
                thread.pushEvent("isCommit", queryRequest.Result);
            }
        }

        public static IEnumerator isSubmission(string submissionHash, Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<IsSubmissionFunction, EthereumTypes.Bool>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new IsSubmissionFunction() { SubmissionHash = Utils.HexStringToByteArray(submissionHash) }, address);
            yield return queryRequest.Result;

            if (thread != null)
            {
                thread.pushEvent("isSubmission", queryRequest.Result);
            }
        }

        public static IEnumerator getTotalBalance(Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<GetTotalBalanceFunction, EthereumTypes.Uint256>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new GetTotalBalanceFunction(), address);
            yield return queryRequest.Result;

            if (thread != null)
            {
                thread.pushEvent("isSubmission", queryRequest.Result);
            }
        }

        public static IEnumerator getTournamentCount(Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<GetTournamentCountFunction, EthereumTypes.Uint256>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new GetTournamentCountFunction(), address);
            yield return queryRequest.Result;

            if (thread != null)
            {
                thread.pushEvent("getTournamentCount", queryRequest.Result);
            }
        }

        public static IEnumerator getTournaments(Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<GetTournamentsFunction, EthereumTypes.AddressArray>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new GetTournamentsFunction(), address);
            yield return queryRequest.Result;

            if (thread != null)
            {
                thread.pushEvent("getTournaments", queryRequest.Result);
            }
        }

        public static IEnumerator getSubmission(string submissionHash, Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<GetSubmissionFunction, MatryxSubmission.SubmissionDataDTO>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new GetSubmissionFunction() { SubmissionHash = Utils.HexStringToByteArray(submissionHash) }, address);
            yield return queryRequest.Result;

            if (thread != null)
            {
                thread.pushEvent("getSubmission", queryRequest.Result);
            }
        }

        public static IEnumerator createTournament(MatryxTournament tournament, Async thread=null)
        {
            TournamentDetails tDetails = new TournamentDetails()
            {
                ContentHash = tournament.contentHash,
                Bounty = tournament.bounty,
                EntryFee = tournament.entryFee,
            };

            var createTournamentFnMsg = new CreateTournamentFunction() { TDetails = tDetails, RDetails = tournament.rounds[0].Details, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice };
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.activePrivateKey);
            yield return transactionRequest.SignAndSendTransaction<CreateTournamentFunction>(createTournamentFnMsg, address);

            var getTransactionStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "createTournament", thread));
            yield return getTransactionStatus;
            yield return getTransactionStatus.result;
        }
    }
}