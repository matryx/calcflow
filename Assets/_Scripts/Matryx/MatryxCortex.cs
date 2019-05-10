using UnityEngine;
using UnityEngine.EventSystems;

using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

using Nanome.Maths.Serializers.JsonSerializer;
using UnityEngine.Networking;
using Nanome.Core;
using System.Numerics;
using UnityEngine.UI;
using System.Text.RegularExpressions;
using Nethereum.ABI.Decoders;

namespace Matryx
{
    public class MatryxCortex : MonoBehaviour
    {
        [SerializeField]
        public MatryxAccountMenu accountMenu;

        public static MatryxCortex Instance { get; private set; }
        public static Serializer serializer = new Serializer();
        public static string cortexURL = "https://cortex-staging.matryx.ai";
        public static string platformInfoURL = cortexURL + "/platform/getInfo";
        public static string userInfoURL = cortexURL + "/user/getInfo";
        public static string tokenInfoURL = cortexURL + "/token/getInfo";
        public static string tournamentsURL = cortexURL + "/tournaments";
        public static string artifactsURL = cortexURL + "/artifacts";

        public static string tournamentURL = cortexURL + "/tournaments/address/";
        public static string roundURL = cortexURL + "/rounds/address";
        public static string submissionURL = cortexURL + "/submissions/";
        public static string mySubmissionsURL = cortexURL + "/submissions/owner/";
        public static string commitURL = cortexURL + "/commits/";

        public static string jsonUploadURL = cortexURL + "/upload/json";
        public static string filesUploadURL = cortexURL + "/upload/files";

        public static List<string> supportedCalcflowCategories = new List<string>();

        public MatryxCortex()
        {
            if(Instance == null)
            {
                Instance = this;
                supportedCalcflowCategories.Add("math");
            }
        }

        void Start()
        {
            queue(InitializeNetworkSettings());
            // Make into one
            queue(InitializeContracts());
            queue(InitiateUser());
            queue(RunTests());
        }

        // Internal queue
        private static readonly Queue<IEnumerator> _queue = new Queue<IEnumerator>();

        private static void queue(IEnumerator action)
        {
            lock (_queue)
            {
                _queue.Enqueue(action);
            }
        }

        void Update()
        {
            lock (_queue)
            {
                while (_queue.Count > 0)
                {
                    var coroutine = _queue.Dequeue();
                    StartCoroutine(coroutine);
                }
            }

            EventSystem system = EventSystem.current;
            if (Input.GetKeyDown(KeyCode.Tab))
            {
                Selectable next = system.currentSelectedGameObject.GetComponent<Selectable>().FindSelectableOnDown();

                if (next != null)
                {

                    InputField inputfield = next.GetComponent<InputField>();
                    if (inputfield != null) inputfield.OnPointerClick(new PointerEventData(system));  //if it's an input field, also set the text caret

                    system.SetSelectedGameObject(next.gameObject, new BaseEventData(system));
                }
                else Debug.Log("next nagivation element not found");
            }
        }

        public static void RunFetchTournaments(long page, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            // Schedule query
            Async main = Async.runInCoroutine(delegate (Async thread, object param)
            {
                return FetchTournaments(page, onSuccess, onError);
            });
        }

        private static IEnumerator FetchTournaments(long page, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            var tournaments = new List<MatryxTournament>();
            var offset = page * 10;
            using (WWW www = new WWW(tournamentsURL))
            {
                yield return www;
                try
                {
                    var response = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                    var data = response["data"] as Dictionary<string, object>;

                    var tournamentList = data["tournaments"] as List<object>;
                    for (int i = 0; i < tournamentList.Count; i++)
                    {

                        var jsonTournament = tournamentList[i] as Dictionary<string, object>;

                        string category = jsonTournament["category"] as string;
                        if (!supportedCalcflowCategories.Contains(category)) { continue; }

                        var tournamentTitle = jsonTournament["title"] as string;
                        var bounty = new BigInteger((long)Convert.ToDouble(jsonTournament["bounty"])) * new BigInteger(1e18);
                        var entryFee = new BigInteger((long)Convert.ToDouble(jsonTournament["entryFee"])) * new BigInteger(1e18);
                        var tournament = new MatryxTournament(jsonTournament["address"] as string, tournamentTitle, bounty, entryFee);
                        tournament.description = jsonTournament["description"] as string;
                        tournament.status = jsonTournament["status"] as string;
                        tournaments.Add(tournament);
                    }

                    onSuccess(tournaments);
                }
                catch (Exception e)
                {
                    Debug.Log(e);
                    onError(tournaments);
                }
            }
        }

        public static void RunFetchTournament(string tournamentAddress, long roundNumber, long page, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            // Schedule query
            Async main = Async.runInCoroutine(delegate (Async thread, object param)
            {
                return FetchTournament(tournamentAddress, roundNumber, page, onSuccess, onError);
            });
        }

        private static IEnumerator FetchTournament(string tournamentAddress, long roundNumber, long page, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            var submissions = new List<MatryxSubmission>();
            var offset = page * 10;

            using (WWW www = new WWW(tournamentURL + tournamentAddress))
            {
                yield return www;
                try
                {
                    var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                    if (jsonObj.ContainsKey("error"))
                    {
                        onError(submissions);
                    }
                    else if (jsonObj.ContainsKey("tournament"))
                    {
                        var tournament = jsonObj["tournament"] as Dictionary<string, object>;
                        var title = tournament["title"] as string;
                        var bounty = Convert.ToDouble(tournament["bounty"] as string);
                        var description = tournament["description"] as string;
                        Debug.Log("description: " + description);
                        var winnersByRound = tournament["winners"] as List<object>;
                        Debug.Log("submissionList count: " + winnersByRound.Count);
                        for (int i = 0; i < winnersByRound.Count; i++)
                        {
                            if (winnersByRound[i] == null)
                            {
                                Debug.Log("round empty. Must be current round: " + i);
                                break;
                            }

                            var roundWinners = winnersByRound[i] as List<object>;
                            for (int j = 0; j < roundWinners.Count; j++)
                            {
                                var winner = roundWinners[i] as Dictionary<string, object>;
                                var submission = new MatryxSubmission(winner["address"] as string);
                                submission.title = winner["title"] as string;
                                submission.dto.Reward = BigInteger.Parse(winner["reward"] as string);
                                submissions.Add(submission);
                            }
                        }
                    }

                    Debug.Log("Fetched submissions: " + submissions.Count);
                    onSuccess(submissions);
                }
                catch (Exception e)
                {
                    Debug.Log(e);
                    onError(null);
                }
            }
        }

        public static void GetSubmission(MatryxSubmission submission, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            queue(submission.get(onSuccess, onError));
        }

        public static void RunFetchMySubmissions(MatryxTournament tournament, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            queue(FetchMySubmissions(tournament, onSuccess, onError));
        }
         
        public static IEnumerator FetchMySubmissions(MatryxTournament tournament, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            var url = mySubmissionsURL + NetworkSettings.activeAccount;
            using (var www = new WWW(url))
            {
                yield return www;
                var submissions = new List<MatryxSubmission>();
                try
                {
                    Debug.Log(www.text);
                    var res = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                    var data = res["data"] as Dictionary<string, object>;
                    var submissionData = data["submissions"] as List<object>;

                    foreach (Dictionary<string, object> submissionDictionary in submissionData)
                    {
                        if (submissionDictionary["tournament"] as string != tournament.address) continue;
                        var title = submissionDictionary["title"] as string;
                        var description = submissionDictionary["description"] as string;
                        var hash = submissionDictionary["hash"] as string;
                        MatryxSubmission submission = new MatryxSubmission(tournament, title, hash, description);
                        submissions.Add(submission);
                    }
                }
                catch (Exception e)
                {
                    Debug.Log(e);
                    onError(submissions);
                }

                onSuccess(submissions);
            }
        }

        public static void RunFetchCommit(string commitContentHash, bool fetchContent, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            queue(FetchCommit(commitContentHash, fetchContent, onSuccess, onError));
        }

        private static IEnumerator FetchCommit(string commitContentHash, bool fetchContent, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            var url = commitURL + "0xfc8443b4fbd56883654e6103a3a643ee072c0d19722c78dde0c437075e5f448b";
            using (var www = new WWW(url))
            {
                yield return www;
                MatryxCommit commit = new MatryxCommit();
                try
                {
                    Debug.Log(www.text);
                    var res = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                    var data = res["data"] as Dictionary<string, object>;
                    var commitData = data["commit"] as Dictionary<string, object>;
                    //if (fetchContent)
                    //{
                    //    var ipfsContentHash = commitData["ipfsContent"] as string;
                    //    var request = new Utils.CoroutineWithData<object>(MatryxCortex.Instance, getIPFSContents(ipfsContentHash));
                    //}

                    //commit.hash = commitData["hash"] as string;
                    //commit.owner = commitData["owner"] as string;
                    //commit.parentHash = commitData["parentHash"] as string;
                    //commit.groupHash = commitData["groupHash"] as string;
                    //commit.ipfsContentHash = commitData["ipfsContent"] as string;
                    //commit.height = BigInteger.Parse(commitData["height"] as string);
                    //commit.value = BigInteger.Parse(commitData["value"] as string);
                    //commit.ownerTotalValue = BigInteger.Parse(commitData["ownerTotalValue"] as string);
                    //commit.totalValue = BigInteger.Parse(commitData["totalValue"] as string);
                    //commit.timestamp = BigInteger.Parse(commitData["timestamp"] as string);

                    //yield return commit;
                }
                catch (Exception e)
                {
                    Debug.Log("Could not fetch details of commit with content: " + commitContentHash);
                    Debug.Log(e);
                }

                onSuccess(commit);
            }
        }

        // UPLOAD SUBMISSION
        public static void RunUploadSubmission(MatryxSubmission submission, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            // Schedule query
            Async submit = Async.runInCoroutine(delegate (Async thread, object param)
            {
                return submission.submit();
            });

            submit.onEvent("submit", onSuccess);
        }

        private static ulong ParseHexString(string hexNumber)
        {
            hexNumber = hexNumber.Replace("0x", string.Empty);
            ulong parsed = ulong.Parse(hexNumber, System.Globalization.NumberStyles.AllowHexSpecifier);
            return parsed;
        }

        public static IEnumerator InitializeNetworkSettings()
        {

            NetworkSettings.network = Config.getString("network", "ropsten");
            NetworkSettings.setActiveAccount(Config.getString("address", ""));
            
            yield break;

            // TODO: Get working (For gas estimation)
            //Dictionary<string, string> postData = new Dictionary<string, string>();
            //postData.Add("jsonrpc", "2.0");
            //postData.Add("method", "eth_getBlockByNumber");
            //postData.Add("params", "['latest',false]");
            //postData.Add("id", "1");
            //UnityWebRequest blockByNumberRequest = UnityWebRequest.Post(NetworkSettings.infuraProvider, postData); //"{\"jsonrpc\":\"2.0\",\"method\":\"eth_getBlockByNumber\",\"params\": [\"0x5BAD55\",false],\"id\":1}"
            //blockByNumberRequest.SetRequestHeader("Content-Type", "application/json");
            //yield return blockByNumberRequest.Send();
            //print("request completed with code: " + blockByNumberRequest.responseCode);
            //if (blockByNumberRequest.isNetworkError)
            //{
            //    print("Error: " + blockByNumberRequest.error);
            //}
            //else
            //{
            //    print("Request Response: " + blockByNumberRequest.downloadHandler.text);
            //}

            //var response = serializer.Deserialize<object>(blockByNumberRequest.downloadHandler.data) as Dictionary<string, object>;

            //var blockGasEstimate = new Nethereum.JsonRpc.Client.RpcRequest(1, "eth_getBlockByNumber", "0x4380d5", false);
            //var request = new Nethereum.JsonRpc.UnityClient.EthCallUnityRequest(MatryxGlobals.infuraProvider);
            //yield return request.SendRequest(blockGasEstimate);

            //MatryxGlobals.txGas = request.Result;
        }

        public static IEnumerator InitiateUser()
        {
            MatryxAccountMenu.UnlockAccount();

            bool settingUp = true;
            while (!NetworkSettings.declinedAccountUnlock.HasValue || settingUp)
            {
                yield return null;
                settingUp = NetworkSettings.activeAccount == null || NetworkSettings.activeAccount == "" || MatryxPlatform.address == null || MatryxToken.address == null || MatryxCommit.address == null || MatryxTournament.ABI == null;
            }

            if (NetworkSettings.declinedAccountUnlock.Value)
            {
                yield break;
            }

            var tokenBalance = new Utils.CoroutineWithData<BigInteger>(MatryxCortex.Instance, MatryxToken.balanceOf(NetworkSettings.activeAccount));
            yield return tokenBalance;
            NetworkSettings.MTXBalance = tokenBalance.result;
            if(MatryxAccountMenu.Instance)
            {
                MatryxAccountMenu.Instance.AccountInfoText[1].text = (NetworkSettings.MTXBalance / new BigInteger(1e18)).ToString() + " MTX";
            }
            yield break;
        }

        public static IEnumerator InitializeContracts()
        {
            using (WWW www = new WWW(artifactsURL))
            {
                yield return www;
                var response = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                var data = response["data"] as Dictionary<string, object>;

                var token = data["token"] as Dictionary<string, object>;
                var tokenAbi = serializer.SerializeString(token["abi"]);
                var tokenAddress = token["address"].ToString();
                if (MatryxToken.contract == null)
                {
                    MatryxToken.setContract(tokenAbi, tokenAddress);
                }

                var platform = data["platform"] as Dictionary<string, object>;
                var platformAbi = serializer.SerializeString(platform["abi"]);
                var platformAddress = platform["address"] as string;
                if (MatryxPlatform.contract == null)
                {
                    MatryxPlatform.setContract(platformAbi, platformAddress);
                }

                var commit = data["commit"] as Dictionary<string, object>;
                var commitAbi = serializer.SerializeString(commit["abi"]);
                var commitAddress = commit["address"] as string;
                if (MatryxCommit.contract == null)
                {
                    MatryxCommit.setContract(commitAbi, commitAddress);
                }

                var tournament = data["tournament"] as Dictionary<string, object>;
                var tournamentAbi = serializer.SerializeString(tournament["abi"]);
                if (MatryxTournament.ABI == null)
                {
                    MatryxTournament.ABI = tournamentAbi;
                }
            }
        }

        public static IEnumerator RunTests()
        {
            bool settingUp = true;
            while (!NetworkSettings.declinedAccountUnlock.HasValue || settingUp)
            {
                yield return null;
                settingUp = NetworkSettings.activeAccount == null || NetworkSettings.activeAccount == "" || MatryxPlatform.address == null || MatryxToken.address == null || MatryxCommit.address == null || MatryxTournament.ABI == null;
            }

            //var request = new Utils.CoroutineWithData<MatryxCommit.Commit>(MatryxCortex.Instance, MatryxCommit.getCommitByContent("QmSFMpah9yQ7YdLCcZAgJq2doFk2G8JxhY7GvbCZowuATq"));
            //yield return request;
            //var hash = request.result.CommitHash;

            //var getCommit = new Utils.CoroutineWithData<MatryxCommit.Commit>(MatryxCortex.Instance, MatryxCommit.getCommit("0xfc8443b4fbd56883654e6103a3a643ee072c0d19722c78dde0c437075e5f448b"));
            //yield return getCommit;
            //var hash = getCommit.result.CommitHash;

            //var request = new Utils.CoroutineWithData<MatryxSubmission.SubmissionDataDTO>(MatryxCortex.Instance, MatryxPlatform.getSubmission("0xe627ceaf65decb29bc6a5f4be54b8d909f28b5ce770fd863c701ff19db28c44d"));
            //yield return request;
            //var hash = request.result.CommitHash;

            //var tournament = new MatryxTournament("0x95405c6fcfcb43d1f11f0318d54e83521be6e7c6");
            //var request = new Utils.CoroutineWithData<EthereumTypes.Uint256>(MatryxCortex.Instance, tournament.getCurrentRoundIndex());
            //yield return request;
            //var hash = request.result;
            //Debug.Log("current round index of tournament " + tournament.address.Substring(0, 10) + " is " + request.result.Value);

            //var tournament = new MatryxTournament("0x95405c6fcfcb43d1f11f0318d54e83521be6e7c6");
            //var request = new Utils.CoroutineWithData<MatryxTournament.TournamentInfo>(MatryxCortex.Instance, tournament.getInfo());
            //yield return request;
            //var hash = request.result;
            //Debug.Log("info for tournament " + tournament.address.Substring(0, 10) + " is " + request.result);

            //var request = new Utils.CoroutineWithData<EthereumTypes.Bool>(MatryxCortex.Instance, MatryxPlatform.isCommit("0xfc8443b4fbd56883654e6103a3a643ee072c0d19722c78dde0c437075e5f448b"));
            //yield return request;
            //var hash = request.result.Value;

            //var getInitialCommits = new Utils.CoroutineWithData<EthereumTypes.Bytes32Array>(MatryxCortex.Instance, MatryxCommit.getInitialCommits());
            //yield return getInitialCommits;
            //Debug.Log("Initial commits: " + getInitialCommits.result.Value);

            //var contentHash = "QmQuw1nGDfjMjKk8qMR4ZCfWHzgwMZvg824uZHDwCUje1i";
            //var request = new Utils.CoroutineWithData<MatryxCommit.Commit>(MatryxCortex.Instance, MatryxCommit.getCommitByContent(contentHash));
            //yield return request;
            //var bytes32Decoder = new StringBytes32Decoder(); //Nethereum.ABI.Decoders
            //var cHash = bytes32Decoder.Decode(request.result.CommitHash);
            //Debug.Log("commitHash 0xfc8443b4 match " + cHash + "?" + cHash.Contains("0xfc8443b4"));

            //var submissionHash = "0xe627ceaf65decb29bc6a5f4be54b8d909f28b5ce770fd863c701ff19db28c44d";
            //var request = new Utils.CoroutineWithData<MatryxSubmission.Submission>(MatryxCortex.Instance, MatryxPlatform.getSubmission(submissionHash));
            //yield return request;

            //var request = new Utils.CoroutineWithData<MatryxCommit.Commit>(MatryxCortex.Instance, MatryxCommit.getInstance());
            //yield return request;
            //var bytes32Decoder = new StringBytes32Decoder(); //Nethereum.ABI.Decoders
            //var cHash = bytes32Decoder.Decode(request.result.CommitHash);
            //Debug.Log("0xf63c37c307779cf621c809125e8353eeace1ea06".Substring(0, 10) + " full commit hash: " + cHash);

            //var request = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, FetchCommit("QmQuw1nGDfjMjKk8qMR4ZCfWHzgwMZvg824uZHDwCUje1i", null));
            //yield return request;

            //MatryxTournament tournament = new MatryxTournament("0x95405c6fcfcb43d1f11f0318d54e83521be6e7c6");
            //RunFetchMySubmissions(tournament, Instance.ProcessSubmissions);

            Debug.Log("Active account set to: " + NetworkSettings.activeAccount);
        }

        private void ProcessSubmissions(object results)
        {
            var submissions = results as List<MatryxSubmission>;
            Debug.Log(submissions[0].title);
        }
    }
}