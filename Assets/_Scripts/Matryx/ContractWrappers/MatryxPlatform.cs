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
            [Parameter("bytes32[3]", 1)]
            public List<string> Title { get; set; }
            [Parameter("bytes32", 4)]
            public string Category { get; set; }
            [Parameter("bytes32[2]", 5)]
            public List<string> DescHash { get; set; }
            [Parameter("bytes32[2]", 7)]
            public List<string> FileHash { get; set; }
            [Parameter("uint256", 9)]
            public BigInteger Bounty { get; set; }
            [Parameter("uint256", 10)]
            public BigInteger EntryFee { get; set; }
        }

        //function getInfo() external view returns(MatryxPlatform.Info);
        public static IEnumerator getInfo(Async thread)
        {
            var queryRequest = new QueryUnityRequest<GetInfoFunction, MatryxPlatformInfoDTO>(Network.infuraProvider, Network.account);
            yield return queryRequest.Query(new GetInfoFunction(), address);
            //Debug.Log("MatryxPlatform info: " + queryRequest.Result);

            thread.pushEvent("getInfo", queryRequest.Result);
        }

        public static IEnumerator hasEnteredMatryx(string user)
        {
            var queryRequest = new QueryUnityRequest<HasEnteredMatryxFunction, EthereumTypes.Bool>(Network.infuraProvider, Network.account);
            yield return queryRequest.Query(new HasEnteredMatryxFunction() { User = user }, address);
            Debug.Log(user + " has entered Matryx? : " + queryRequest.Result.Value);

            //thread.pushEvent("hasEnteredMatryx", queryRequest.Result);
        }

        public static IEnumerator getUsers(Async thread, uint offset, uint count)
        {
            var queryRequest = new QueryUnityRequest<GetUsersFunction, EthereumTypes.AddressArray>(Network.infuraProvider, Network.account);
            yield return queryRequest.Query(new GetUsersFunction() { Offset = offset, Count = count }, address);
            //Debug.Log("users: " + queryRequest.Result.Value);

            thread.pushEvent("getUsers", queryRequest.Result);
        }

        public static IEnumerator enterMatryx(Async thread)
        {
            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            var enterMatryxFunctionMessage = new EnterMatryxFunction() { Gas = Network.txGas, GasPrice = Network.txGasPrice };

            yield return transactionRequest.SignAndSendTransaction<EnterMatryxFunction>(enterMatryxFunctionMessage, address);

            //yield return Utils.HandleSignedSentTransaction(thread, "enterMatryx", transactionRequest);
        }

        // TODO: Test
        public static IEnumerator createTournament(Async thread, string title, string category, string descHash, string fileHash, BigInteger bounty, BigInteger entryFee, BigInteger roundStart, BigInteger roundEnd, BigInteger roundReview, BigInteger roundBounty)
        {
            TournamentDetails tDetails = new TournamentDetails() { Title = Utils.stringToBytes(title, 3), Category = Utils.stringToBytes(category, 1)[0], DescHash = Utils.stringToBytes(descHash, 2), FileHash = Utils.stringToBytes(fileHash, 2), Bounty = bounty, EntryFee = entryFee };
            MatryxRound.RoundDetails rDetails = new MatryxRound.RoundDetails() { Start = roundStart, End = roundEnd, Review = roundReview, Bounty = roundBounty };
            var createTournamentFnMsg = new CreateTournamentFunction() { TDetails = tDetails, RDetails = rDetails, Gas = Network.txGas, GasPrice = Network.txGasPrice };

            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            yield return transactionRequest.SignAndSendTransaction<CreateTournamentFunction>(new CreateTournamentFunction() { TDetails = tDetails, RDetails = rDetails }, address);

            //yield return Utils.HandleSignedSentTransaction(thread, "createTournament", transactionRequest);
        }
    }
}