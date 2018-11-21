using Nethereum.Hex.HexTypes;
using Nethereum.JsonRpc.UnityClient;
using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Numerics;
using UnityEngine;
using Nanome.Core;

namespace Matryx
{
    public class Network
    {
        public static string networkName = "ropsten";
        public static string infuraProvider = "https://" + networkName + ".infura.io/metamask";
        public static string privateKey = "";
        public static string account = "";
        public static HexBigInteger txGas = new HexBigInteger(new BigInteger(3e6));
        public static HexBigInteger txGasPrice = new HexBigInteger(new BigInteger(5e9));
    }

    public class Utils
    {
        public static List<string> stringToBytes(string text, int len = 0)
        {
            var bytesTitle = new List<string>();

            if (text == null)
            {
                text = "";
            }

            var hexConverter = new Nethereum.Hex.HexConvertors.HexUTF8StringConvertor();
            var data = hexConverter.ConvertToHex(text);
            data = data.Substring(2);

            if (len <= 0)
            {
                bytesTitle.Add(data);
                return bytesTitle;
            }

            var padding = (64 - (data.Length % 64));
            data = data.PadRight(data.Length + padding, '0');

            var rx = new Regex(".{1,64}");
            MatchCollection matches = rx.Matches(data);

            var matchEnumerator = matches.GetEnumerator();
            for (int i = 0; i < matches.Count; i++)
            {
                matchEnumerator.MoveNext();
                bytesTitle.Add("0x" + matchEnumerator.Current as string);
            }

            while (bytesTitle.Count < len)
            {
                bytesTitle.Add("0x00");
            }

            Debug.Log("bytesTitle: " + bytesTitle.ToArray());

            return bytesTitle;
        }

        public static IEnumerator HandleSignedSentTransaction(TransactionSignedUnityRequest transactionRequest)
        {
            if (transactionRequest.Exception != null)
            {
                Debug.Log(transactionRequest.Exception.Message);
                yield break;
            }

            var transactionHash = transactionRequest.Result;

            Debug.Log(transactionRequest.GetType().ToString() + " transaction hash: " + transactionHash);

            var transactionReceiptPolling = new TransactionReceiptPollingRequest(Network.infuraProvider);
            //checking every 2 seconds for the receipt
            yield return transactionReceiptPolling.PollForReceipt(transactionHash, 2);
            var transactionReceipt = transactionReceiptPolling.Result;

            Debug.Log(transactionRequest.GetType().ToString() + " succeeded? : " + transactionReceipt.Status.Value);
            //if(transactionReceipt.Status.Value == 0x1)
            //{
            //    thread.pushEvent(txName+"-success", transactionHash);
            //}
            //else
            //{
            //    thread.pushEvent(txName+"-failure", transactionHash);
            //}
        }
    }
}