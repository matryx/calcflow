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
    public static class MatryxToken
    {
        public static string address;
        public static string ABI;
        public static Contract contract;

        [Function("getOwner", "address")]
        public class GetOwnerFunction : FunctionMessage { }

        [Function("balanceOf", "uint256")]
        public class BalanceOfFunction : FunctionMessage
        {
            [Parameter("address", "_owner", 1)]
            public string Owner { get; set; }
        }
        [FunctionOutput]
        public class BalanceOfFunctionOutput : IFunctionOutputDTO
        {
            [Parameter("uint256", 1)]
            public BigInteger Balance { get; set; }
        }

        [Function("approve")]
        public class ApproveFunction : FunctionMessage
        {
            [Parameter("address")]
            public string Spender { get; set; }
            [Parameter("uint256")]
            public BigInteger Amount { get; set; }
        }

        public static IEnumerator getOwner()
        {
            var queryRequest = new QueryUnityRequest<GetOwnerFunction, EthereumTypes.Address>(Network.infuraProvider, Network.account);
            yield return queryRequest.Query(new GetOwnerFunction() { }, MatryxToken.address);
            //Debug.Log("getOwner result: " + queryRequest.Result);
        }

        public static IEnumerator balanceOf(string owner)
        {
            var queryRequest = new QueryUnityRequest<BalanceOfFunction, BalanceOfFunctionOutput>(Network.infuraProvider, Network.account);
            yield return queryRequest.Query(new BalanceOfFunction() { Owner = owner }, MatryxToken.address);

            //Getting the dto response already decoded
            var dtoResult = queryRequest.Result;
            //Debug.Log("balance of " + owner + ":" + dtoResult.Balance);
        }

        public static IEnumerator approve(string spender, BigInteger amount)
        {
            var transactionRequest = new TransactionSignedUnityRequest(Network.infuraProvider, Network.privateKey, Network.account);
            var approveFnMsg = new ApproveFunction() { Spender = spender, Amount = amount, Gas = Network.txGas, GasPrice = Network.txGasPrice };
            yield return transactionRequest.SignAndSendTransaction<ApproveFunction>(approveFnMsg, address);
            yield return Utils.HandleSignedSentTransaction(transactionRequest);
        }
    }
}