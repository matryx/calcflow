using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Nethereum.ABI.FunctionEncoding.Attributes;
using Nethereum.Contracts;
using Nethereum.JsonRpc.UnityClient;
using System.Numerics;
using Nanome.Core;

// TODO: all of this
namespace Matryx
{
    public static class MatryxUser 
    {
        public static string address;
        public static string ABI;
        public static Contract contract;

        [Function("getData", typeof(MatryxUserDataDTO))]
        public class GetDataFunction : FunctionMessage
        {
            [Parameter("address", "user", 1)]
            public string User { get; set; }
        }

        [Function("getSubmissions", typeof(EthereumTypes.AddressArray))]
        public class GetSubmissionsFunction : FunctionMessage
        {
            [Parameter("address", "user", 1)]
            public string User { get; set; }
        }

        [Function("getSubmissionsByTournament", typeof(EthereumTypes.AddressArray))]
        public class GetSubmissionsByTournamentFunction : FunctionMessage
        {
            [Parameter("address", "user", 1)]
            public string User { get; set; }
            [Parameter("address", "tAddress", 1)]
            public string Tournament { get; set; }
        }


        [FunctionOutput]
        public class MatryxUserDataDTO : IFunctionOutputDTO
        {
            [Parameter("bool")]
            public static BigInteger exists { get; set; }
            [Parameter("uint256")]
            public static BigInteger timeEntered { get; set; }
            [Parameter("uint256")]
            public static BigInteger positiveVotes { get; set; }
            [Parameter("uint256")]
            public static BigInteger negativeVotes { get; set; }
            [Parameter("uint256")]
            public static BigInteger totalSpent { get; set; }
            [Parameter("uint256")]
            public static BigInteger totalWinnings { get; set; }
            [Parameter("address[]")]
            public static List<string> tournaments { get; set; }
            [Parameter("address[]")]
            public static List<string> tournamentsEntered { get; set; }
            [Parameter("address[]")]
            public static List<string> submissions { get; set; }
            [Parameter("address[]")]
            public static List<string> contributedTo { get; set; }
            [Parameter("address[]")]
            public static List<string> unlockedFiles { get; set; }
        }

        public static IEnumerator getData(Async thread=null)
        {
            var queryRequest = new QueryUnityRequest<GetDataFunction, MatryxUserDataDTO>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return queryRequest.Query(new GetDataFunction() { User = NetworkSettings.address }, MatryxUser.address);
            yield return queryRequest.Result;
        }

        public static IEnumerator getSubmissions(Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<GetSubmissionsFunction, EthereumTypes.AddressArray>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return queryRequest.Query(new GetSubmissionsFunction() { User = NetworkSettings.address }, MatryxUser.address);
            yield return queryRequest.Result.Value;
        }

        public static IEnumerator getSubmissionsByTournament(string tournamentAddress, Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<GetSubmissionsByTournamentFunction, EthereumTypes.AddressArray>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return queryRequest.Query(new GetSubmissionsByTournamentFunction() { User = NetworkSettings.address, Tournament = tournamentAddress }, MatryxUser.address);
            yield return queryRequest.Result.Value;
        }
    }
}