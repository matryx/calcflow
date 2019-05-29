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
using System.Security.Cryptography;
using System.Drawing;
using System.IO;
using System.Drawing.Imaging;
using Nethereum.Web3.Accounts;

namespace Matryx
{
    public class NetworkSettings
    {
        public static bool? declinedAccountUnlock;

        public static string network = "ropsten";
        public static string infuraProvider = "https://ropsten.infura.io/v3/2373e82fc83341ff82b66c5a87edd5f5";
        public static Nethereum.HdWallet.Wallet mnemonicWallet;
        public static Dictionary<string, string> aggregateWallet = new Dictionary<string, string>();
        static string _currentAddress = "";
        // All accounts the network settings know about
        public static string[] accounts
        {
            get
            {
                if (mnemonicWallet != null)
                {
                    return mnemonicWallet.GetAddresses(10);
                }
                else return aggregateWallet.Keys.ToArray();
            }
        }

        // The address that transactions are currently being made by
        public static string currentAddress
        {
            get
            {
                if (mnemonicWallet != null && mnemonicWallet.GetAccount(_currentAddress) != null)
                {
                    return Nethereum.Util.AddressExtensions.ConvertToEthereumChecksumAddress(_currentAddress);
                }
                else if (aggregateWallet.Count > 0 && !_currentAddress.Equals(string.Empty))
                {
                    return Nethereum.Util.AddressExtensions.ConvertToEthereumChecksumAddress(_currentAddress);
                }
                else
                {
                    return null;
                }
            }
        }

        // The private key currently being used to make transactions
        public static string currentPrivateKey
        {
            get
            {
                if (mnemonicWallet != null)
                {
                    return mnemonicWallet.GetAccount(_currentAddress).PrivateKey;
                }
                else if (aggregateWallet.Count > 0 && !_currentAddress.Equals(string.Empty))
                {
                    return aggregateWallet[_currentAddress];
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

        public static bool importWallet(string mnemonic, string password="", bool makeActive = true)
        {
            mnemonicWallet = new Nethereum.HdWallet.Wallet(mnemonic, password);
            if(makeActive)
            {
                _currentAddress = mnemonicWallet.GetAccount(0).Address;
            }

            return true;
        }

        public static bool importKey(string privateKey, bool makeActive = true)
        {
            string address = Nethereum.Signer.EthECKey.GetPublicAddress(privateKey);
            aggregateWallet.Add(address, privateKey);
            if(makeActive)
            {
                _currentAddress = address;
            }

            return true;
        }

        public static bool importKeystore(string keystore, string password, bool makeActive = true)
        {
            var service = new Nethereum.KeyStore.KeyStoreService();
            var sK = service.DecryptKeyStoreFromJson(password, keystore);
            string privateKey = "0x" + System.BitConverter.ToString(sK).Replace("-", "");
            string address = Nethereum.Signer.EthECKey.GetPublicAddress(privateKey);
            aggregateWallet.Add(address, privateKey);
            if (makeActive)
            {
                _currentAddress = address;
            }

            return true;
        }

        public static void setActiveAccount(string address)
        {
            if (aggregateWallet.ContainsKey(address))
            {
                _currentAddress = address;
                Debug.Log("Active account set to: " + _currentAddress);
            }
            else if (mnemonicWallet != null)
            {
                Account account = mnemonicWallet.GetAccount(address);
                if (account.Address != null)
                {
                    _currentAddress = account.Address;
                    Debug.Log("Active account set to: " + _currentAddress);
                }
            }
            else
            {
                throw new System.Exception("Could not find address (" + address + ") in wallet.");
            }
        }

        public static void SignOut()
        {
            // Clear wallets
            aggregateWallet.Clear();
            mnemonicWallet = null;
            TournamentsMenu.SetState(TournamentsMenu.TournamentMenuState.AccountUnlockRequired);
        }
    }

    

    public class Utils
    {
        public class Accounts
        {
            public static Texture2D getBlockieTexture(string accountAddress)
            {
                var blockies = new NetBlockies.Blockies(accountAddress);
                return blockies.GetTexture();
            }

            public static string ellipseAddress(string address)
            {
                return address.Substring(0, 6) + "..." + address.Substring(address.Length - 5, 4);
            }
        }

        public class Time
        {
            private static readonly DateTime epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
            public static DateTime FromUnixTime(double unixTime)
            {
                return epoch.AddSeconds(unixTime);
            }

            public static DateTime FromUnixTime(BigInteger unixTime)
            {
                var timestring = unixTime.ToString();
                double timeAsDouble = Convert.ToDouble(timestring);
                return epoch.AddSeconds(timeAsDouble);
            }

            public static DateTime FromUnixTime(string unixTime)
            {
                return epoch.AddSeconds(double.Parse(unixTime));
            }

            public static double ToUnixTime(DateTime time)
            {
                TimeSpan duration = time - epoch;
                return duration.TotalSeconds;
            }
        }

        public static string Substring(string toSubstring, char first, char last)
        {
            var openIndex = toSubstring.IndexOf(first);
            var closeIndex = toSubstring.LastIndexOf(last);
            if(openIndex != -1 && closeIndex != -1)
            {
                return toSubstring.Substring(openIndex, closeIndex - openIndex + 1);
            }
            else
            {
                return "";
            }
        }

        public static string GetMultiHash(byte[] data)
        {
            SHA256 sha = SHA256.Create();
            var digest = sha.ComputeHash(data);
            var multihash = new byte[digest.Length + 2];
            multihash[0] = 0x12;
            multihash[1] = 0x20;
            digest.CopyTo(multihash, 2);
            return Crypto.B58Encode(multihash);
        }

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

        public static string SHA256(string data)
        {
            StringBuilder Sb = new StringBuilder();
            using (SHA256 hash = SHA256Managed.Create())
            {
                Encoding enc = Encoding.UTF8;
                byte[] result = hash.ComputeHash(enc.GetBytes(data));

                foreach (byte b in result)
                {
                    Sb.Append(b.ToString("x"));
                }
            }

            return Sb.ToString();
        }

        public static readonly char[] _alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".ToCharArray();

        public static string B58Encode(byte[] input)
        {
            if (0 == input.Length)
            {
                return String.Empty;
            }
            input = CopyOfRange(input, 0, input.Length);
            // Count leading zeroes.
            int zeroCount = 0;
            while (zeroCount < input.Length && input[zeroCount] == 0)
            {
                zeroCount++;
            }
            // The actual encoding.
            byte[] temp = new byte[input.Length * 2];
            int j = temp.Length;

            int startAt = zeroCount;
            while (startAt < input.Length)
            {
                byte mod = DivMod58(input, startAt);
                if (input[startAt] == 0)
                {
                    startAt++;
                }
                temp[--j] = (byte)_alphabet[mod];
            }

            // Strip extra '1' if there are some after decoding.
            while (j < temp.Length && temp[j] == _alphabet[0])
            {
                ++j;
            }
            // Add as many leading '1' as there were leading zeros.
            while (--zeroCount >= 0)
            {
                temp[--j] = (byte)_alphabet[0];
            }

            byte[] output = CopyOfRange(temp, j, temp.Length);
            try
            {
                return Encoding.ASCII.GetString(output);
            }
            catch (DecoderFallbackException e)
            {
                Console.WriteLine(e.ToString());
                return String.Empty;
            }
        }

        static byte DivMod58(byte[] number, int startAt)
        {
            int remainder = 0;
            for (int i = startAt; i < number.Length; i++)
            {
                int digit256 = (int)number[i] & 0xFF;
                int temp = remainder * 256 + digit256;

                number[i] = (byte)(temp / 58);

                remainder = temp % 58;
            }

            return (byte)remainder;
        }

        static byte DivMod256(byte[] number58, int startAt)
        {
            int remainder = 0;
            for (int i = startAt; i < number58.Length; i++)
            {
                int digit58 = (int)number58[i] & 0xFF;
                int temp = remainder * 58 + digit58;

                number58[i] = (byte)(temp / 256);

                remainder = temp % 256;
            }

            return (byte)remainder;
        }

        static byte[] CopyOfRange(byte[] source, int from, int to)
        {
            byte[] range = new byte[to - from];
            for (int i = 0; i < to - from; i++)
            {
                range[i] = source[from + i];
            }
            return range;
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