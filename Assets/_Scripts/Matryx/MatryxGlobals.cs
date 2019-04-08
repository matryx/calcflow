using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Numerics;
using System.Text;
using System.Linq;
using System.Net.Http;

using UnityEngine;
using UnityEngine.Networking;

using Nanome.Core;

using Nethereum.Hex.HexTypes;
using Nethereum.JsonRpc.UnityClient;

using Assets.USecurity;
using Nanome.Maths.Serializers.JsonSerializer;
using System;

namespace Matryx
{
    public class NetworkSettings
    {
        public static bool? declinedAccountUnlock;

        public static string network = "ropsten";
        public static string infuraProvider = "https://ropsten.infura.io/v3/2373e82fc83341ff82b66c5a87edd5f5";
        public static Nethereum.HdWallet.Wallet wallet;
        public static List<string> _accounts = new List<string>();
        public static List<string> _privateKeys = new List<string>();
        static int activeAccountIndex = 0;
        // All accounts the network settings know about
        public static string[] accounts
        {
            get
            {
                if (wallet != null)
                {
                    return wallet.GetAddresses(10);
                }
                else return _accounts.ToArray();
            }
        }
        // The account that transactions are currently being made under
        public static string activeAccount
        {
            get
            {
                if (wallet != null)
                {
                    return wallet.GetAccount(0).Address;
                }
                else if (_accounts.Count > 0)
                {
                    return _accounts.ElementAt(activeAccountIndex);
                }
                else
                {
                    return null;
                }
            }
        }
        // The private key currently being used to make transactions
        public static string activePrivateKey
        {
            get
            {
                if (wallet != null)
                {
                    return "0x" + System.BitConverter.ToString(wallet.GetPrivateKey(0)).Replace("-", "");
                }
                else if (_privateKeys.Count > 0)
                {
                    return _privateKeys[activeAccountIndex];
                }
                else
                {
                    return null;
                }
            }
        }
        public static BigInteger MTXBalance;
        public static HexBigInteger txGas = new HexBigInteger(new BigInteger(3e6));
        public static HexBigInteger txGasPrice = new HexBigInteger(new BigInteger(5e9));

        // Importing a wallet currently overrides all imported single private keys
        // (including those from keystore files)
        public static bool importWallet(string mnemonic, string password=null)
        {
            wallet = new Nethereum.HdWallet.Wallet(mnemonic, "");
            return true;
        }

        public static bool importKey(string privateKey, bool makeActive = true)
        {
            _privateKeys.Add(privateKey);
            _accounts.Add(Nethereum.Signer.EthECKey.GetPublicAddress(privateKey));
            if(makeActive)
            {
                activeAccountIndex = _privateKeys.Count - 1;
            }

            return true;
        }

        public static bool importKeystore(string keystore, string password, bool makeActive = true)
        {
            var service = new Nethereum.KeyStore.KeyStoreService();
            var sK = service.DecryptKeyStoreFromJson(password, keystore);
            _privateKeys.Add("0x" + System.BitConverter.ToString(sK).Replace("-", ""));
            _accounts.Add(Nethereum.Signer.EthECKey.GetPublicAddress(_privateKeys.Last()));
            return true;
        }

        public static void setActiveAccount(string address)
        {
            activeAccountIndex = _accounts.IndexOf(address);
            activeAccountIndex = activeAccountIndex == -1 ? 0 : activeAccountIndex;
        }
    }

    public class Utils
    {
        static System.Random random = new System.Random();
        public static string GetRandomHexNumber(int digits)
        {
            byte[] buffer = new byte[digits / 2];
            random.NextBytes(buffer);
            string result = String.Concat(buffer.Select(x => x.ToString("X2")).ToArray());
            if (digits % 2 == 0)
                return result;
            return result + random.Next(16).ToString("X");
        }

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

        public static string BytesArrayToString(byte[] hexBytes)
        {
            var chars = Encoding.Unicode.GetChars(hexBytes);
            var theString = new string(chars);
            return theString;
        }

        public static byte[] HexStringToByteArray(string hexString)
        {
            if(hexString[0] == '0' && hexString[1] == 'x')
            {
                hexString = hexString.Substring(2, hexString.Length - 2);
            }

            if (hexString.Length % 2 == 1)
                throw new System.Exception("The binary key cannot have an odd number of digits");

            byte[] retval = new byte[hexString.Length / 2];
            for (int i = 0; i < hexString.Length; i += 2)
                retval[i / 2] = Convert.ToByte(hexString.Substring(i, 2), 16);
            return retval;
        }

        public static int GetHexVal(char hex)
        {
            int val = (int)hex;
            //For uppercase A-F letters:
            return val - (val < 58 ? 48 : 55);
            //For lowercase a-f letters:
            //return val - (val < 58 ? 48 : 87);
            //Or the two combined, but a bit slower:
            //return val - (val < 58 ? 48 : (val < 97 ? 55 : 87));
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
            var txReceipt = new CoroutineWithData<Nethereum.RPC.Eth.DTOs.TransactionReceipt>(MatryxCortex.Instance, GetTransactionReceipt(transactionRequest, eventName, thread));
            yield return txReceipt;
            yield return txReceipt.result.Status.Value == new BigInteger(1);
            Debug.Log("result of " + eventName + ": " + txReceipt.result.Status.Value);
        }

        public static IEnumerator uploadFiles(List<string> fileNames, List<byte[]> contents, List<string> fileTypes, Async thread = null)
        {
            WWWForm form = new WWWForm();
            for (var i = 0; i < fileNames.Count; i++)
            {
                form.AddBinaryData("files", contents[i], fileNames[i], fileTypes[i]);
            }

            UnityWebRequest request = UnityWebRequest.Post(MatryxCortex.filesUploadURL, form);
            yield return request.SendWebRequest();
            Debug.Log("request completed with code: " + request.responseCode);
            if (request.isNetworkError || request.responseCode != 200)
            {
                Debug.Log("Error: " + request.error);
            }
            else
            {
                Debug.Log("Request Response: " + request.downloadHandler.text);
            }

            var response = MatryxCortex.serializer.Deserialize<object>(request.downloadHandler.data) as Dictionary<string, object>;
            var data = response["data"] as Dictionary<string, object>;
            string multiHash = data["hash"] as string;

            if(thread != null)
            {
                thread.pushEvent("uploadToIPFS-success", multiHash);
            }

            yield return multiHash;
        }

        private static readonly HttpClient client = new HttpClient();
        private static Serializer serializer = new Serializer();
        public static IEnumerator uploadJson(string title, string description, string ipfsFiles, string category="math", Async thread = null)
        {
            Dictionary<string, string> jsonDictionary = new Dictionary<string, string>()
            {
                {"title", title },
                {"description", description },
                {"category", "math" },
                { "ipfsFiles", ipfsFiles }
            };

            UnityWebRequest request = UnityWebRequest.Post(MatryxCortex.jsonUploadURL, jsonDictionary);
            yield return request.SendWebRequest();

            if (request.isNetworkError || request.responseCode != 200)
            {
                Debug.Log("Error: " + request.error);
            }
            else
            {
                Debug.Log("Request Response: " + request.downloadHandler.text);
            }

            var res = MatryxCortex.serializer.Deserialize<object>(request.downloadHandler.data) as Dictionary<string, object>;
            var data = res["data"] as Dictionary<string, object>;
            var multiHash = data["hash"] as string;

            yield return multiHash;

            if (thread != null)
            {
                thread.pushEvent("uploadToIPFS-success", multiHash);
            }
        }

        /// <summary>
        /// Uploads a Submission's description and files to IPFS.
        /// </summary>
        /// <param name="submission"> The submission whose description and json content are to be uploaded. </param>
        /// <returns> The description hash and json content hash of the submission in that order. </returns>
        public static IEnumerator uploadSubmission(MatryxSubmission submission)
        {
            string descriptionHash = "";
            string jsonContentHash = "";

            if (submission.data.ContentHash == null || submission.data.ContentHash.Equals(string.Empty))
            {
                if (submission.description != null && !submission.description.Equals(string.Empty))
                {
                    var uploadToIPFS = new Utils.CoroutineWithData<string>(MatryxCortex.Instance, Utils.uploadJson(submission.title, submission.description, submission.commit.ipfsContentHash));
                    yield return uploadToIPFS;
                    descriptionHash = uploadToIPFS.result;
                }
            }

            yield return new string[2] { descriptionHash, jsonContentHash };
        }

        /// <summary>
        /// Uploads a Tournament's description and files to IPFS.
        /// </summary>
        /// <param name="tournament"> The tournament whose description and files are to be uploaded. </param>
        /// <returns> The description hash and files hash of the tournament in that order. </returns>
        public static IEnumerator uploadTournament(MatryxTournament tournament)
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

            if (tournament.contentHash == string.Empty)
            {
                if (tournament.description != null && !tournament.description.Equals(string.Empty))
                {
                    var uploadToIPFS = new Utils.CoroutineWithData<string>(MatryxCortex.Instance, Utils.uploadJson(tournament.title, tournament.description, ""));
                    yield return uploadToIPFS;
                    contentHash = uploadToIPFS.result;
                }
            }

            yield return new string[2] { contentHash, filesHash };
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

        public static string Decrypt(string cypher, string password = "")
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