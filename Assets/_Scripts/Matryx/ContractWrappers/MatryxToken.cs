using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Nethereum.ABI.FunctionEncoding.Attributes;
using Nethereum.Contracts;
using System.Numerics;
using Nanome.Core;
using Nethereum.JsonRpc.UnityClient;

namespace Matryx
{
    public static class MatryxToken
    {
        public static string address;
        public static string ABI;
        public static Contract contract;

        public static void setContract(string abi, string addr)
        {
            address = addr;
            ABI = abi;
            contract = new Nethereum.Contracts.Contract(null, abi, addr);
        }

        [Function("getOwner", "address")]
        public class GetOwnerFunction : FunctionMessage { }

        [Function("balanceOf", "uint256")]
        public class BalanceOfFunction : FunctionMessage
        {
            [Parameter("address", "_owner", 1)]
            public string Owner { get; set; }
        }

        [Function("allowance")]
        public class AllowanceFunction : FunctionMessage
        {
            [Parameter("address")]
            public string Owner { get; set; }
            [Parameter("address")]
            public string Spender { get; set; }
        }

        [Function("approve")]
        public class ApproveFunction : FunctionMessage
        {
            [Parameter("address")]
            public string Spender { get; set; }
            [Parameter("uint256")]
            public BigInteger Amount { get; set; }
        }

        public static IEnumerator getOwner(Async thread)
        {
            var queryRequest = new QueryUnityRequest<GetOwnerFunction, EthereumTypes.Address>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new GetOwnerFunction() { }, MatryxToken.address);
            //Debug.Log("getOwner result: " + queryRequest.Result);
        }

        public static IEnumerator balanceOf(string owner, Async thread=null)
        {
            var queryRequest = new QueryUnityRequest<BalanceOfFunction, EthereumTypes.Uint256>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new BalanceOfFunction() { Owner = owner }, MatryxToken.address);
            yield return queryRequest.Result.Value;
        }

        public static IEnumerator allowance(string owner, string spender, Async thread=null)
        {
            var queryRequest = new QueryUnityRequest<AllowanceFunction, EthereumTypes.Uint256>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            var allowanceFnMessage = new AllowanceFunction() { Owner = owner, Spender = spender };
            yield return queryRequest.Query(allowanceFnMessage, MatryxToken.address);
            yield return queryRequest.Result.Value;

            if (thread != null)
            {
                thread.pushEvent("allowance", queryRequest.Result.Value);
            }
        }

        public static IEnumerator approve(string spender, BigInteger amount, Async thread=null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.activePrivateKey);
            var approveFnMsg = new ApproveFunction() { Spender = spender, Amount = amount, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice };
            yield return transactionRequest.SignAndSendTransaction<ApproveFunction>(approveFnMsg, address);

            var txStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "approve", thread));
            yield return txStatus;
            yield return txStatus.result;
        }
    }
}