using System.Collections;
using System.Numerics;
using System.Collections.Generic;
using UnityEngine;
using Nethereum.Contracts;
using Nethereum.ABI.FunctionEncoding.Attributes;
using Nethereum.JsonRpc.UnityClient;
using Nanome.Core;

// TODO: all of this
namespace Matryx
{
    public static class MatryxPlatform
    {
        public static string address;
        public static string ABI;
        public static Nethereum.Contracts.Contract contract;

        [Function("getInfo", typeof(MatryxPlatformInfoDTO))]
        public class GetInfoFunction : FunctionMessage { }

        [FunctionOutput]
        public class MatryxPlatformInfoDTO : IFunctionOutputDTO
        {
            [Parameter("address")]
            public static string System { get; set; }
            [Parameter("uint256")]
            public static BigInteger Version { get; set; }
            [Parameter("address")]
            public static string Token { get; set; }
            [Parameter("address")]
            public static string Owner { get; set; }
        }

        [Function("hasEnteredMatryx", "bool")]
        public class HasEnteredMatryxFunction : FunctionMessage
        {
            [Parameter("address", 1)]
            public string User { get; set; }
        }

        [Function("getUsers", "address[]")]
        public class GetUsersFunction : FunctionMessage
        {
            [Parameter("uint256", 1)]
            public BigInteger Offset { get; set; }
            [Parameter("uint256", 2)]
            public BigInteger Count { get; set; }
        }

        [Function("enterMatryx")]
        public class EnterMatryxFunction : FunctionMessage { }

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
            [Parameter("bytes32[3]")]
            public List<string> Title { get; set; }
            [Parameter("bytes32")]
            public string Category { get; set; }
            [Parameter("bytes32[2]")]
            public List<string> DescHash { get; set; }
            [Parameter("bytes32[2]")]
            public List<string> FileHash { get; set; }
            [Parameter("uint256")]
            public BigInteger Bounty { get; set; }
            [Parameter("uint256")]
            public BigInteger EntryFee { get; set; }
        }

        //function getInfo() external view returns(MatryxPlatform.Info);
        public static IEnumerator getInfo(Async thread=null)
        {
            var queryRequest = new QueryUnityRequest<GetInfoFunction, MatryxPlatformInfoDTO>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return queryRequest.Query(new GetInfoFunction(), address);
            yield return queryRequest.Result;

            if (thread != null)
            {
                thread.pushEvent("getInfo", queryRequest.Result);
            }
        }

        public static IEnumerator hasEnteredMatryx(string user, Async thread=null)
        {
            var queryRequest = new QueryUnityRequest<HasEnteredMatryxFunction, EthereumTypes.Bool>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return queryRequest.Query(new HasEnteredMatryxFunction() { User = user }, address);
            yield return queryRequest.Result.Value;

            if (thread != null)
            {
                thread.pushEvent("hasEnteredMatryx", queryRequest.Result.Value);
            }
        }

        public static IEnumerator getUsers(uint offset, uint count, Async thread=null)
        {
            var queryRequest = new QueryUnityRequest<GetUsersFunction, EthereumTypes.AddressArray>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return queryRequest.Query(new GetUsersFunction() { Offset = offset, Count = count }, address);
            yield return queryRequest.Result.Value;

            if (thread != null)
            {
                thread.pushEvent("getUsers", queryRequest.Result.Value);
            }
        }

        public static IEnumerator enterMatryx(Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.privateKey, NetworkSettings.address);
            var enterMatryxFunctionMessage = new EnterMatryxFunction() { Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice };

            yield return transactionRequest.SignAndSendTransaction<EnterMatryxFunction>(enterMatryxFunctionMessage, address);

            var txStatus = new Utils.CoroutineWithData<bool>(MatryxExplorer.Instance, Utils.GetTransactionStatus(transactionRequest, "enterMatryx", thread));
            yield return txStatus;
            yield return txStatus.result;
        }

        public static IEnumerator createTournament(MatryxTournament tournament, Async thread=null)
        {
            TournamentDetails tDetails = new TournamentDetails()
            {
                Title = Utils.stringToString32(tournament.title, 3),
                Category = Utils.stringToString32(tournament.category, 1)[0],
                DescHash = Utils.stringToString32(tournament.descHash, 2),
                FileHash = Utils.stringToString32(tournament.fileHash, 2),
                Bounty = tournament.bounty,
                EntryFee = tournament.entryFee,
            };

            var createTournamentFnMsg = new CreateTournamentFunction() { TDetails = tDetails, RDetails = tournament.rounds[0].Details, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice };
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.privateKey, NetworkSettings.address);
            yield return transactionRequest.SignAndSendTransaction<CreateTournamentFunction>(createTournamentFnMsg, address);

            var getTransactionStatus = new Utils.CoroutineWithData<bool>(MatryxExplorer.Instance, Utils.GetTransactionStatus(transactionRequest, "createTournament", thread));
            yield return getTransactionStatus;
            yield return getTransactionStatus.result;
        }
    }
}