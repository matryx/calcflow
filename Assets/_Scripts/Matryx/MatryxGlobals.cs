using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Numerics;
using System.Text;
using System.Linq;

using UnityEngine;
using UnityEngine.Networking;

using Nanome.Core;

using Nethereum.Hex.HexTypes;
using Nethereum.JsonRpc.UnityClient;

using Assets.USecurity;

namespace Matryx
{
    public class NetworkSettings
    {
        public static bool? declinedAccountUnlock;

        public static string network = "ropsten";
        public static string infuraProvider = "https://" + network + ".infura.io/metamask";
        public static string address;
        public static string privateKey;
        public static BigInteger MTXBalance;
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

        public static List<string> stringToString32(string text, int len = 0)
        {
            text = text.PadRight(len * 32, '\0');
            return Enumerable.Range(0, len).Select(i => text.Substring(i * 32, 32)).ToList();
        }

        public static string bytesToString(string[] text)
        {
            string title = "";

            if (text == null)
            {
                return title;
            }

            var hexConverter = new Nethereum.Hex.HexConvertors.HexUTF8StringConvertor();
            for (int i = 0; i < text.Length; i++)
            {
                title += hexConverter.ConvertFromHex(text[i]);
            }

            return title;
        }

        public static IEnumerator GetTransactionReceipt(TransactionSignedUnityRequest transactionRequest, string eventName, Async thread=null)
        {
            if (transactionRequest.Exception != null)
            {
                Debug.Log(transactionRequest.Exception.Message);
                yield break;
            }

            var transactionHash = transactionRequest.Result;
            Debug.Log(eventName + " transaction hash: " + transactionHash);

            var transactionReceiptPolling = new TransactionReceiptPollingRequest(NetworkSettings.infuraProvider);
            yield return transactionReceiptPolling.PollForReceipt(transactionHash, 2);
            yield return transactionReceiptPolling.Result;

            if (thread != null)
            {
                string suffix = transactionReceiptPolling.Result.Status.Value == new BigInteger(1) ? "-success" : "-failure";
                thread.pushEvent(eventName + suffix, transactionReceiptPolling.Result);
            }
        }

        public static IEnumerator GetTransactionStatus(TransactionSignedUnityRequest transactionRequest, string eventName, Async thread = null)
        {
            var txReceipt = new CoroutineWithData<Nethereum.RPC.Eth.DTOs.TransactionReceipt>(MatryxExplorer.Instance, GetTransactionReceipt(transactionRequest, eventName, thread));
            yield return txReceipt;
            yield return txReceipt.result.Status.Value == new BigInteger(1);
            Debug.Log("result for " + eventName + ": " + txReceipt.result.Status);
        }

        public static IEnumerator uploadToIPFS(string fieldName, string extension, string content, string contentType, Async thread=null)
        {
            WWWForm form = new WWWForm();
            form.AddBinaryData(fieldName, Encoding.ASCII.GetBytes(content.ToCharArray()), fieldName+extension, contentType);
            UnityWebRequest ipfsRequest = UnityWebRequest.Post(MatryxExplorer.uploadURL, form);
            yield return ipfsRequest.SendWebRequest();
            Debug.Log("request completed with code: " + ipfsRequest.responseCode);
            if (ipfsRequest.isNetworkError)
            {
                Debug.Log("Error: " + ipfsRequest.error);
            }
            else
            {
                Debug.Log("Request Response: " + ipfsRequest.downloadHandler.text);
            }

            var response = MatryxExplorer.serializer.Deserialize<object>(ipfsRequest.downloadHandler.data) as Dictionary<string, object>;
            var multiHash = response[response.Keys.ToArray()[0]];

            yield return multiHash;

            if(thread != null)
            {
                thread.pushEvent("uploadToIPFS-success", multiHash);
            }
        }

        /// <summary>
        /// Uploads a Submission's description and files to IPFS.
        /// </summary>
        /// <param name="submission"> The submission whose description and json content are to be uploaded. </param>
        /// <returns> The description hash and json content hash of the submission in that order. </returns>
        public static IEnumerator uploadToIPFS(MatryxSubmission submission)
        {
            string descriptionHash = "";
            string jsonContent = "";

            if(submission.details.descHash == null || submission.details.descHash.Equals(string.Empty))
            {
                if(submission.description != null && !submission.description.Equals(string.Empty))
                {
                    var uploadToIPFS = new Utils.CoroutineWithData<string>(MatryxExplorer.Instance, Utils.uploadToIPFS("description", ".txt", "This submission was created with Calcflow.", "text/plain"));
                    yield return uploadToIPFS;
                    descriptionHash = uploadToIPFS.result;
                }
            }

            if(submission.details.fileHash == null || submission.details.fileHash.Equals(string.Empty))
            {
                if(submission.file != null && !submission.file.Equals(string.Empty))
                {
                    var uploadToIPFS = new Utils.CoroutineWithData<string>(MatryxExplorer.Instance, Utils.uploadToIPFS("jsonContent", ".json", submission.file, "application/json"));
                    yield return uploadToIPFS;
                    jsonContent = uploadToIPFS.result;
                }
            }

            yield return new string[2] { descriptionHash, jsonContent };
        }

        /// <summary>
        /// Uploads a Tournament's description and files to IPFS.
        /// </summary>
        /// <param name="tournament"> The tournament whose description and files are to be uploaded. </param>
        /// <returns> The description hash and files hash of the tournament in that order. </returns>
        public static IEnumerator uploadToIPFS(MatryxTournament tournament)
        {
            string descriptionHash = "";
            string filesHash = "";

            if (tournament.descHash == null || tournament.descHash.Equals(string.Empty))
            {
                if (tournament.description != null && !tournament.description.Equals(string.Empty))
                {
                    var uploadToIPFS = new Utils.CoroutineWithData<string>(MatryxExplorer.Instance, Utils.uploadToIPFS("description", ".txt", tournament.description, "text/plain"));
                    yield return uploadToIPFS;
                    descriptionHash = uploadToIPFS.result;
                }
            }

            if (tournament.fileHash == null || tournament.fileHash.Equals(string.Empty))
            {
                if (tournament.file != null && !tournament.file.Equals(string.Empty))
                {
                    var uploadToIPFS = new Utils.CoroutineWithData<string>(MatryxExplorer.Instance, Utils.uploadToIPFS("filesContent", "", tournament.file, "text/plain"));
                    yield return uploadToIPFS;
                    filesHash = uploadToIPFS.result;
                }
            }

            yield return new string[2] { descriptionHash, filesHash };
        }

        public class CoroutineWithData<T> : CustomYieldInstruction
        {
            private IEnumerator _target;
            public T result;
            public bool done = false;
            public Coroutine Coroutine { get; private set; }

            public CoroutineWithData(MonoBehaviour owner_, IEnumerator target_)
            {
                _target = target_;
                Coroutine = owner_.StartCoroutine(Run());
            }

            private IEnumerator Run()
            {
                while (_target.MoveNext())
                {
                    if (_target.Current is T)
                    {
                        result = (T)_target.Current;
                        done = true;
                    }
                    yield return _target.Current;
                }
            }

            public override bool keepWaiting
            {
                get
                {
                    return !done;
                }
            }
        }
    }

    public static class Crypto
    {
        public static string Encrypt(string data, string password=null)
        {
            return AES.Encrypt(System.Text.Encoding.UTF8.GetBytes(data), password);
        }

        public static string Decrypt(string cypher, string password = null)
        {
            return AES.Decrypt(cypher, password);
        }
    }

    // Public api
    public delegate void ResultDelegate(object obj);

    public class RoutineContext
    {
        public object param;
        public ResultDelegate callback;

        public RoutineContext(object param, ResultDelegate callback)
        {
            this.param = param;
            this.callback = callback;
        }

        public void done(object result)
        {
            if (this.callback != null)
            {
                this.callback(result);
            }
        }
    }
}