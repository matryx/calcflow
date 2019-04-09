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

namespace Matryx
{
    public class MatryxCortex : MonoBehaviour
    {
        [SerializeField]
        public MatryxAccountMenu accountMenu;

        public static MatryxCortex Instance { get; private set; }
        public static Serializer serializer = new Serializer();
        public static string cortexURL = "https://cortex-staging.matryx.ai"; //"https://explorer-dev.matryx.ai";
        public static string platformInfoURL = cortexURL + "/platform/getInfo";
        public static string userInfoURL = cortexURL + "/user/getInfo";
        public static string tokenInfoURL = cortexURL + "/token/getInfo";
        public static string tournamentsURL = cortexURL + "/tournaments";
        public static string artifactsURL = cortexURL + "/artifacts";

        public static string tournamentURL = cortexURL + "/tournaments/address/";
        public static string roundURL = cortexURL + "/rounds/address";
        public static string submissionURL = cortexURL + "/submissions/hash/";
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

        public static void RunFetchTournaments(long page, Async.EventDelegate callback)
        {
            // Schedule query
            Async main = Async.runInCoroutine(delegate (Async thread, object param)
            {
                return FetchTournaments(page, callback);
            });
        }

        private static IEnumerator FetchTournaments(long page, Async.EventDelegate callback)
        {
            var tournaments = new List<MatryxTournament>();
            var offset = page * 10;
            using (WWW www = new WWW(tournamentsURL))
            {
                yield return www;
                var res = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                var data = res["data"] as Dictionary<string, object>;
                var tournamentList = data["tournaments"] as List<object>;
                for (int i = 0; i < tournamentList.Count; i++)
                {
                    try
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
                    catch (Exception e) { Debug.Log(e); }
                }
                callback(tournaments);
            }
        }

        public static void RunFetchTournament(string tournamentAddress, long roundNumber, long page, Async.EventDelegate callback)
        {
            // Schedule query
            Async main = Async.runInCoroutine(delegate (Async thread, object param)
            {
                return FetchTournament(tournamentAddress, roundNumber, page, callback);
            });
        }

        private static IEnumerator FetchTournament(string tournamentAddress, long roundNumber, long page, Async.EventDelegate callback)
        {
            var submissions = new List<MatryxSubmission>();
            var offset = page * 10;

            using (WWW www = new WWW(tournamentURL + tournamentAddress))
            {
                yield return www;
                // Debug.Log(www.text);
                var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                if (jsonObj.ContainsKey("error"))
                {
                    callback(submissions);
                }
                else if(jsonObj.ContainsKey("tournament"))
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

                        try
                        {
                            var roundWinners = winnersByRound[i] as List<object>;
                            for (int j = 0; j < roundWinners.Count; j++)
                            {
                                var winner = roundWinners[i] as Dictionary<string, object>;
                                var submission = new MatryxSubmission(winner["address"] as string);
                                submission.title = winner["title"] as string;
                                submission.data.Reward = BigInteger.Parse(winner["reward"] as string);
                                submissions.Add(submission);
                            }
                        }
                        catch (System.ArgumentOutOfRangeException e) { Debug.Log(e); break; }
                        catch (Exception e) { Debug.Log(e); }
                    }

                    Debug.Log("Fetched submissions: " + submissions.Count);
                    callback(submissions);
                }
            }
        }

        // DETAIL SUBMISSION
        public static void RunFetchSubmission(MatryxSubmission submission, Async.EventDelegate callback)
        {
            queue(FetchSubmission(submission, callback));
        }

        private static IEnumerator FetchSubmission(MatryxSubmission submission, Async.EventDelegate callback)
        {
            var url = submissionURL + submission.hash;
            using (var www = new WWW(url))
            {
                yield return www;
                Debug.Log(www.text);
                var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                var jsonSubmission = jsonObj["submission"] as Dictionary<string, object>;
                try
                {
                    submission.commit.owner = jsonSubmission["owner"] as string;
                    var timestamp = BigInteger.Parse(jsonSubmission["timestamp"] as string);
                    submission.data.Timestamp = timestamp;
                    submission.description = jsonSubmission["description"] as string;
                    callback(submission);
                }
                catch (Exception e)
                {
                    Debug.Log("Could not read submission at:" + submission.hash + " tournament: " + submission.tournament.address);
                    Debug.Log(e);
                    callback(null);
                }
            }
        }

        public static void RunFetchMySubmissions(MatryxTournament tournament, Async.EventDelegate callback)
        {
            queue(FetchMySubmissions(tournament, callback));
        }
         
        public static IEnumerator FetchMySubmissions(MatryxTournament tournament, Async.EventDelegate callback)
        {
            //var mySubmissionsOnTournamentCoroutine = new Utils.CoroutineWithData<List<string>>(Instance, MatryxUser.getSubmissionsByTournament(tournament.address));
            //yield return mySubmissionsOnTournamentCoroutine;

            //var tournamentSubmissions = mySubmissionsOnTournamentCoroutine.result;
            //var submissions = new List<MatryxSubmission>();
            //for(int i = 0; i < tournamentSubmissions.Count; i++)
            //{
            //    MatryxSubmission submission = new MatryxSubmission(tournamentSubmissions[i]);
            //    var submissionTitleCoroutine = new Utils.CoroutineWithData<string>(Instance, submission.get());
            //    yield return submissionTitleCoroutine;

            //    submissions.Add(submission);
            //}

            //callback(submissions);

            yield return null;
        }

        // UPLOAD SUBMISSION
        public static void RunUploadSubmission(MatryxSubmission submission, Async.EventDelegate callback)
        {
            // Schedule query
            Async submit = Async.runInCoroutine(delegate (Async thread, object param)
            {
                return submission.submit();
            });

            submit.onEvent("submit", callback);
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
                var res = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                var data = res["data"] as Dictionary<string, object>;

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

            var commitHash = "0xfc8443b4fbd56883654e6103a3a643ee072c0d19722c78dde0c437075e5f448b";
            var request = new Utils.CoroutineWithData<MatryxCommit.Commit>(MatryxCortex.Instance, MatryxCommit.getCommit(commitHash));
            yield return request;
            Debug.Log("is " + commitHash.Substring(0, 10) + " a commit? " + request.result.Value);
        }
    }
}