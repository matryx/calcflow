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
using Nanome.Core.Extension;
using System.Reflection;

namespace Matryx
{
    public class MatryxCortex : MonoBehaviour
    {
        [SerializeField]
        public MatryxAccountMenu accountMenu;
        [SerializeField]
        public ExpressionSaveLoad expressionSaveLoad;

        public static MatryxCortex Instance { get; private set; }
        public static Serializer serializer = new Serializer();
        public static string cortexURL = "https://cortex.matryx.ai";
        public static string platformInfoURL = cortexURL + "/platform/getInfo";
        public static string userInfoURL = cortexURL + "/user/getInfo";
        public static string tokenInfoURL = cortexURL + "/token/getInfo";
        public static string tournamentsURL = cortexURL + "/tournaments?count=100&offset=0&sortBy=round_end&status=open&category=math";
        public static string myTournamentsURL = cortexURL + "/tournaments?offset=0&sortBy=round_end&category=math&owner=";
        public static string artifactsURL = cortexURL + "/artifacts";

        public static string tournamentURL = cortexURL + "/tournaments/";
        public static string roundURL = cortexURL + "/rounds/address";
        public static string submissionURL = cortexURL + "/submissions/";
        public static string mySubmissionsURL = cortexURL + "/submissions/owner/";
        public static string commitURL = cortexURL + "/commits/";
        public static string myCommitsURL = commitURL + "owner/";

        public static string ipfsURL = "https://ipfs.infura.io:5001/api/v0";
        public static string ipfsObjURL = ipfsURL + "/object/get?arg=";
        public static string ipfsCatURL = ipfsURL + "/cat?arg=";
        public static string ipfsAddURL = ipfsURL + "/add?pin=false";

        public static string jsonUploadURL = cortexURL + "/upload/json";
        public static string filesUploadURL = ipfsURL + "/add?recursive=true&quieter=true"; //&wrap-with-directory=true

        public static List<string> supportedCalcflowCategories = new List<string>();

        [SerializeField]
        GameObject claimCommitButton;

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
            if (!MatryxAccountMenu.UnlockAccount())
            {
                TournamentsMenu.SetState(TournamentsMenu.TournamentMenuState.AccountUnlockRequired);
            }

            queue(InitializeNetworkSettings());
            queue(InitializeContracts());
            queue(InitializeUser());

            queue(RunTests());
        }

        // Internal queue
        private static readonly Queue<IEnumerator> _queue = new Queue<IEnumerator>();
        private static readonly Queue<IEnumerator> _meteredQueue = new Queue<IEnumerator>();

        private static void queue(IEnumerator action)
        {
            lock (_queue)
            {
                _queue.Enqueue(action);
            }
        }

        public static void queueMetered(IEnumerator action)
        {
            lock(_meteredQueue)
            {
                _meteredQueue.Enqueue(action);
                Instance.StartCoroutine(CoroutineCascade());
            }
        }

        public static IEnumerator CoroutineCascade()
        {
            if(_meteredQueue.Count == 0)
            {
                yield return null;
            }

            if (_meteredQueue.Count > 0)
            {
                yield return _meteredQueue.Dequeue();
            }

            Instance.StartCoroutine(CoroutineCascade());
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

        public static void GetMTXBalance(Async.EventDelegate onSuccess)
        {
            Instance.StartCoroutine(MTXBalanceCoroutine(onSuccess));
        }

        public static IEnumerator MTXBalanceCoroutine(Async.EventDelegate onSuccess)
        {
            var tokenBalanceCoroutine = new Utils.CoroutineWithData<BigInteger>(MatryxCortex.Instance, MatryxToken.balanceOf(NetworkSettings.currentAddress));
            yield return tokenBalanceCoroutine;
            var balance = tokenBalanceCoroutine.result / new BigInteger(1e18);
            onSuccess(balance);
        }

        public static void RunGetTournaments(long page, float waitTime, Async.EventDelegate onSuccess, Async.EventDelegate onError = null)
        {
            // Schedule query
            Async main = Async.runInCoroutine(delegate (Async thread, object param)
            {
                return GetTournaments(page, waitTime, false, onSuccess, onError);
            });
        }

        public static void RunGetMyTournaments(long page, float waitTime, Async.EventDelegate onSuccess, Async.EventDelegate onError = null)
        {
            // Schedule query
            Async main = Async.runInCoroutine(delegate (Async thread, object param)
            {
                return GetTournaments(page, waitTime, true, onSuccess, onError);
            });
        }

        private static IEnumerator GetTournaments(long page, float waitTime, bool onlyMine, Async.EventDelegate onSuccess, Async.EventDelegate onError = null)
        {
            if (waitTime > 0)
            {
                yield return new WaitForSeconds(waitTime);
            }

            var tournaments = new List<MatryxTournament>();
            var offset = page * 10;
            var url = onlyMine ? myTournamentsURL + NetworkSettings.currentAddress : tournamentsURL;
            using (WWW www = new WWW(url))
            {
                yield return www;
                if (www.error != null)
                {
                    Debug.Log("Error making request. Matryx Cortex down!!");
                    onError?.Invoke(www.error);
                    yield break;
                }
                
                var response = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                var data = response["data"] as Dictionary<string, object>;

                var tournamentList = data["tournaments"] as List<object>;
                for (int i = 0; i < tournamentList.Count; i++)
                {
                    var jsonTournament = tournamentList[i] as Dictionary<string, object>;
                    var jsonRound = jsonTournament["round"] as Dictionary<string, object>;

                    string category = jsonTournament["category"] as string;
                    if (!supportedCalcflowCategories.Contains(category)) { continue; }

                    var owner = jsonTournament["owner"] as string;
                    if (onlyMine && !owner.Equals(NetworkSettings.currentAddress, StringComparison.CurrentCultureIgnoreCase))
                    {
                        continue;
                    }
                    else if (!onlyMine)
                    {
                        var status = jsonRound["status"] as string;
                        if (!status.Equals("open", StringComparison.CurrentCultureIgnoreCase)) { continue; }
                    }

                    var tournamentTitle = jsonTournament["title"] as string;
                    var bounty = new BigInteger((long)Convert.ToDouble(jsonTournament["bounty"])) * new BigInteger(1e18);
                    var entryFee = new BigInteger((long)Convert.ToDouble(jsonTournament["entryFee"])) * new BigInteger(1e18);
                    var tournament = new MatryxTournament(jsonTournament["address"] as string, tournamentTitle, bounty, entryFee);
                    tournament.description = jsonTournament["description"] as string;
                    tournament.owner = owner;

                    var idx = Convert.ToInt32(jsonRound["index"] as string);
                    var roundStart = DateTime.Parse(jsonRound["startDate"] as string);
                    var roundEnd = DateTime.Parse(jsonRound["endDate"] as string);
                    var roundReviewEnd = DateTime.Parse(jsonRound["reviewEndDate"] as string);
                    var roundBounty = Convert.ToDecimal(jsonRound["bounty"]);
                    var roundParticipants = Convert.ToInt32(jsonRound["totalParticipants"]);
                    var roundSubmissions = Convert.ToInt32(jsonRound["totalSubmissions"]);
                    tournament.currentRound = new MatryxRound()
                    {
                        tournament = tournament,
                        index = idx,
                        startDate = roundStart,
                        endDate = roundEnd,
                        reviewEndDate = roundReviewEnd,
                        Bounty = roundBounty,
                        totalParticipants = roundParticipants,
                        totalSubmissions = roundSubmissions
                    };

                    tournaments.Add(tournament);
                }

                onSuccess?.Invoke(tournaments);
            }
        }

        public static void RunGetTournament(string tournamentAddress, bool getDescription, Async.EventDelegate onSuccess, Async.EventDelegate onError = null)
        {
            // Schedule query
            Async main = Async.runInCoroutine(delegate (Async thread, object param)
            {
                return GetTournament(tournamentAddress, getDescription, onSuccess, onError);
            });
        }

        private static IEnumerator GetTournament(string tournamentAddress, bool getDescription, Async.EventDelegate onSuccess, Async.EventDelegate onError = null)
        {
            var tournament = new MatryxTournament(tournamentAddress);
            var winningSubmissions = new List<MatryxSubmission>();

            using (WWW www = new WWW(tournamentURL + tournamentAddress))
            {
                yield return www;
                if (www.error != null)
                {
                    Debug.Log("Error making request. Matryx Cortex down!!");
                    onError?.Invoke(www.error);
                    yield break;
                }

                try
                {
                    var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                    if (jsonObj.ContainsKey("error"))
                    {
                        onError?.Invoke(null);
                    }
                    else if (jsonObj.ContainsKey("success"))
                    {
                        var data = jsonObj["data"] as Dictionary<string, object>;
                        var jsonTournament = data["tournament"] as Dictionary<string, object>;
                        tournament.owner = jsonTournament["owner"] as string;
                        tournament.contentHash = jsonTournament["ipfsContent"] as string;
                        tournament.title = jsonTournament["title"] as string;
                        tournament.Bounty = new BigInteger(Convert.ToDecimal(jsonTournament["bounty"]));
                        tournament.description = jsonTournament["description"] as string;

                        var jsonRound = jsonTournament["round"] as Dictionary<string, object>;
                        tournament.currentRound = new MatryxRound(Convert.ToInt32(jsonRound["index"]));
                        var idx = Convert.ToInt32(jsonRound["index"] as string);
                        var roundClosed = (jsonRound["status"] as string).Equals("closed");
                        var roundStart = DateTime.Parse(jsonRound["startDate"] as string);
                        var roundEnd = DateTime.Parse(jsonRound["endDate"] as string);
                        BigInteger duration = new BigInteger((roundEnd - roundStart).TotalSeconds);
                        var roundReviewEnd = DateTime.Parse(jsonRound["reviewEndDate"] as string);
                        var roundBounty = Convert.ToDecimal(jsonRound["bounty"]);
                        var roundParticipants = Convert.ToInt32(jsonRound["totalParticipants"]);
                        var roundSubmissions = Convert.ToInt32(jsonRound["totalSubmissions"]);
                        tournament.currentRound = new MatryxRound()
                        {
                            tournament = tournament,
                            index = idx,
                            closed = roundClosed,
                            startDate = roundStart,
                            endDate = roundEnd,
                            reviewEndDate = roundReviewEnd,
                            Bounty = roundBounty,
                            totalParticipants = roundParticipants,
                            totalSubmissions = roundSubmissions
                        };

                        tournament.currentRound.Details.Start = new BigInteger(Utils.Time.ToUnixTime(roundStart));
                        tournament.currentRound.Details.Duration = duration;

                        Dictionary<string, MatryxSubmission> submissionDictionary = new Dictionary<string, MatryxSubmission>();
                        if (roundEnd < DateTime.Now)
                        {
                            var tournamentSubmissions = jsonRound["submissions"] as List<object>;
                            for (int i = 0; i < tournamentSubmissions.Count; i++)
                            {
                                var jsonSubmission = tournamentSubmissions[i] as Dictionary<string, object>;
                                var subHash = jsonSubmission["hash"] as string;
                                var subOwner = jsonSubmission["owner"] as string;
                                var subTitle = jsonSubmission["title"] as string;
                                var subDesc = jsonSubmission["description"] as string;
                                var subReward = Convert.ToInt32(jsonSubmission["reward"] as string);
                                var subTimestamp = Convert.ToDecimal(jsonSubmission["timestamp"] as string);

                                var submission = new MatryxSubmission(tournament, subTitle, subHash, subDesc)
                                {
                                    owner = subOwner,
                                    Reward = subReward,
                                    Timestamp = subTimestamp
                                };

                                tournament.currentRound.allSubmissions.Add(submission);
                                submissionDictionary.Add(submission.hash, submission);
                            }
                        }
                        
                        var roundWinners = jsonRound["winners"] as List<object>;
                        for (int i = 0; i < roundWinners.Count; i++)
                        {
                            var winningSubmission = submissionDictionary[roundWinners[i] as string];
                            winningSubmissions.Add(winningSubmission);
                        }

                        tournament.currentRound.winningSubmissions = winningSubmissions;
                        onSuccess(tournament);
                    }
                }
                catch (Exception e)
                {
                    Debug.Log(e);
                    onError?.Invoke(null);
                }
            }
        }

        public static void RunGetRound(MatryxTournament tournament, int roundIndex, Async.EventDelegate onSuccess, Async.EventDelegate onError = null)
        {
            Async main = Async.runInCoroutine(delegate (Async thread, object param)
            {
                return GetRound(tournament, roundIndex, onSuccess, onError);
            });
        }

        public static IEnumerator GetRound(MatryxTournament tournament, int roundIndex, Async.EventDelegate onSuccess, Async.EventDelegate onError = null)
        {
            var round = new MatryxRound(roundIndex);
            round.tournament = tournament;
            var winningSubmissions = new List<MatryxSubmission>();

            using (WWW www = new WWW(tournamentURL + tournament.address + "/round/" + roundIndex))
            {
                yield return www;
                if (www.error != null)
                {
                    Debug.Log("Error making request. Matryx Cortex down!!");
                    onError?.Invoke(www.error);
                    yield break;
                }

                try
                {
                    var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                    if (jsonObj.ContainsKey("error"))
                    {
                        onError?.Invoke(null);
                    }
                    else if (jsonObj.ContainsKey("success"))
                    {
                        var data = jsonObj["data"] as Dictionary<string, object>;
                        var jsonRound = data["round"] as Dictionary<string, object>;
                        round.Details.Bounty = new BigInteger(Convert.ToDecimal(jsonRound["bounty"])) * new BigInteger(1e18);
                        var startDate = DateTime.Parse(jsonRound["startDate"] as string);
                        var endDate = DateTime.Parse(jsonRound["endDate"] as string);
                        round.Details.Start = new BigInteger(Utils.Time.ToUnixTime(startDate));
                        round.Details.Duration = new BigInteger((endDate - startDate).TotalSeconds);

                        var roundWinners = jsonRound["winners"] as List<object>;
                        for (int i = 0; i < roundWinners.Count; i++)
                        {
                            var submission = new MatryxSubmission(roundWinners[i] as string);
                            winningSubmissions.Add(submission);
                        }
                        round.winningSubmissions = winningSubmissions;

                        onSuccess(round);
                    }
                }
                catch (System.Exception e)
                {
                    Debug.Log(e);
                    onError?.Invoke(null);
                }
            }
        }

        public static void GetSubmission(MatryxSubmission submission, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            queue(submission.get(onSuccess, onError));
        }

        public static void RunGetMySubmissions(MatryxTournament tournament, float waitTime = 0, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            queue(GetMySubmissions(tournament, waitTime, onSuccess, onError));
        }
         
        public static IEnumerator GetMySubmissions(MatryxTournament tournament, float waitTime, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            if (waitTime > 0)
            {
                yield return new WaitForSeconds(waitTime);
            }

            var url = mySubmissionsURL + NetworkSettings.currentAddress;
            using (var www = new WWW(url))
            {
                yield return www;
                var submissions = new List<MatryxSubmission>();
                try
                {
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
                    onError?.Invoke(submissions);
                }

                onSuccess?.Invoke(submissions);
            }
        }

        public static void RunGetCommit(string commitHash, bool getContent, Async.EventDelegate onSuccess, Async.EventDelegate onError = null)
        {
            queue(GetCommit(commitHash, getContent, onSuccess, onError));
        }

        private static IEnumerator GetCommit(string commitHash, bool getContent, Async.EventDelegate onSuccess, Async.EventDelegate onError = null)
        {
            var url = commitURL + commitHash;
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

                    commit.hash = commitData["hash"] as string;
                    commit.owner = commitData["owner"] as string;
                    commit.parentHash = commitData["parentHash"] as string;
                    commit.ipfsContentHash = commitData["ipfsContent"] as string;
                    commit.height = BigInteger.Parse(commitData["height"].ToString());
                    commit.value = BigInteger.Parse(commitData["value"].ToString());
                    commit.ownerTotalValue = BigInteger.Parse(commitData["ownerTotalValue"].ToString());
                    commit.totalValue = BigInteger.Parse(commitData["totalValue"].ToString());
                    commit.timestamp = BigInteger.Parse(commitData["timestamp"].ToString());

                    if (getContent)
                    {
                        RunGetJSONContent(commit.ipfsContentHash, 
                            (cont) => 
                            {
                                commit.content = Utils.Substring(cont as string, '{', '}');
                                onSuccess?.Invoke(commit);
                            }, onError);
                    }
                    else
                    {
                        onSuccess?.Invoke(commit);
                    }
                }
                catch (Exception e)
                {
                    Debug.Log("Could not fetch details of commit with content: " + commitHash);
                    Debug.Log(e);
                    onError?.Invoke(null);
                }

                yield return commit;
            }
        }

        public static void RunGetMyCommits(Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            queue(GetMyCommits(onSuccess, onError));
        }

        private static IEnumerator GetMyCommits(Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            var url = myCommitsURL + NetworkSettings.currentAddress;
            using (var www = new WWW(url))
            {
                yield return www;
                if(www.error != null)
                {
                    Debug.Log("Error getting commits: " + www.error);
                    onError?.Invoke(www.error);
                    yield break;
                }

                var commits = new List<MatryxCommit>();
                try
                {
                    Debug.Log(www.text);
                    var res = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                    var data = res["data"] as Dictionary<string, object>;
                    var jsonCommits = data["commits"] as List<object>;

                    foreach (Dictionary<string, object> jsonCommit in jsonCommits)
                    {
                        MatryxCommit commit = new MatryxCommit();
                        commit.hash = jsonCommit["hash"] as string;
                        commit.owner = jsonCommit["owner"] as string;
                        commit.parentHash = jsonCommit["parentHash"] as string;
                        commit.ipfsContentHash = jsonCommit["ipfsContent"] as string;
                        commit.height = BigInteger.Parse(jsonCommit["height"].ToString());
                        commit.value = BigInteger.Parse(jsonCommit["value"].ToString());
                        commit.ownerTotalValue = BigInteger.Parse(jsonCommit["ownerTotalValue"].ToString());
                        commit.totalValue = BigInteger.Parse(jsonCommit["totalValue"].ToString());
                        commit.timestamp = BigInteger.Parse(jsonCommit["timestamp"].ToString());
                        commit.mine = true;
                        commits.Add(commit);
                    }
                }
                catch (Exception e)
                {
                    Debug.Log(e);
                    onError?.Invoke(commits);
                }

                onSuccess?.Invoke(commits);
            }
        }

        public static IEnumerator uploadFiles(List<string> fileNames, List<byte[]> contents, List<string> fileTypes, string urlModifier = "", Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            WWWForm form = new WWWForm();
            for (var i = 0; i < fileNames.Count; i++)
            {
                form.AddBinaryData("files", contents[i], fileNames[i], fileTypes[i]);
            }

            UnityWebRequest request = UnityWebRequest.Post(filesUploadURL+urlModifier, form);
            yield return request.SendWebRequest();
            Debug.Log("request completed with code: " + request.responseCode);
            if (request.isNetworkError || request.responseCode != 200)
            {
                Debug.Log("Error: " + request.error);
                onError?.Invoke(request);
            }
            else
            {
                var res = serializer.Deserialize<object>(request.downloadHandler.data) as Dictionary<string, object>;
                string multiHash = res["Hash"] as string;

                onSuccess?.Invoke(multiHash);
                yield return multiHash;
            }

            yield return "You should never see this :)";
        }

        public static IEnumerator uploadJson(string title, string description, string ipfsFiles, string category = "", Async thread = null)
        {
            Dictionary<string, string> jsonDictionary = new Dictionary<string, string>()
            {
                {"title", title },
                {"description", description },
                { "ipfsFiles", ipfsFiles }
            };

            if (category != "")
            {
                jsonDictionary.Add("category", category);
            }

            UnityWebRequest request = UnityWebRequest.Post(jsonUploadURL, jsonDictionary);
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

        public static void RunGetJSONContent(string ipfsHash, Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            queueMetered(GetIPFSFilesContent(ipfsHash, "jsonContent.json", onSuccess, onError));
        }

        //public static IEnumerator GetIPFSJSONContent(string ipfsHash, string elementName = "", Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        //{
        //    var objectURL = ipfsObjURL + ipfsHash;
        //    using (var contwww = new WWW(objectURL))
        //    {
        //        yield return contwww;
        //        try
        //        {
        //            var res = serializer.Deserialize<object>(contwww.bytes) as Dictionary<string, object>;
        //            var data = res["Data"] as string;

        //            var jsonRegex = "{.*}";
        //            var json = data.

                    
        //        }
        //        catch(System.Exception e)
        //        {
        //            onError("Could not fetch json: " + e);
        //        }
        //    }
        //}

        public static IEnumerator GetIPFSFilesContent(string ipfsHash, string elementName = "", Async.EventDelegate onSuccess = null, Async.EventDelegate onError = null)
        {
            var objectURL = ipfsObjURL + ipfsHash;
            Dictionary<string, object> res;
            List<object> links;
            int index;
            Dictionary<string, object> theEntry;
            string linkUrl = "";

            bool error = false;
            bool link = false;

            using (var contwww = new WWW(objectURL))
            {
                yield return contwww;
                try
                {
                    res = serializer.Deserialize<object>(contwww.bytes) as Dictionary<string, object>;
                    links = res["Links"] as List<object>;
                    index = links.IndexOfElementWithValue(elementName);
                    if(index != -1)
                    {
                        link = true;
                        theEntry = links[index] as Dictionary<string, object>;
                        linkUrl = ipfsCatURL + theEntry["Hash"] as string;
                    }
                    else
                    {
                        onSuccess(res["Data"]);
                    }
                }
                catch(System.Exception e)
                {
                    onError?.Invoke(null);
                    error = true;
                }
            }

            if (error) yield break;

            var catURL = link ? linkUrl : ipfsCatURL + ipfsHash;
            using (var itemWWW = new WWW(catURL))
            {
                yield return itemWWW;
                onSuccess?.Invoke(itemWWW.text);
                yield break;
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
            string address = Config.getString("address", "");
            if (!address.Equals(""))
            {
                NetworkSettings.setActiveAccount(address);
            }
            
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

        public static IEnumerator InitializeUser()
        {
            bool settingUp = true;
            while (!NetworkSettings.declinedAccountUnlock.HasValue || settingUp)
            {
                yield return null;
                settingUp = NetworkSettings.currentAddress == null || NetworkSettings.currentAddress == "" || MatryxPlatform.address == null || MatryxToken.address == null || MatryxCommit.address == null || MatryxTournament.ABI == null;
            }

            if (NetworkSettings.declinedAccountUnlock.Value)
            {
                yield break;
            }

            var tokenBalance = new Utils.CoroutineWithData<BigInteger>(MatryxCortex.Instance, MatryxToken.balanceOf(NetworkSettings.currentAddress));
            yield return tokenBalance;
            NetworkSettings.MTXBalance = tokenBalance.result;
            if(MatryxAccountMenu.Instance)
            {
                MatryxAccountMenu.Instance.AccountInfoText[1].text = (NetworkSettings.MTXBalance / new BigInteger(1e18)).ToString() + " MTX";
            }

            MatryxCommit.loadLocalClaims();
            yield return GetMyCommits(MatryxCommit.LoadCommits, (obj) => { Tippies.HeadsetModal("Could not load commits"); });

            yield break;
        }

        public static IEnumerator InitializeContracts(Async.EventDelegate onError = null)
        {
            using (WWW www = new WWW(artifactsURL))
            {
                yield return www;
                if (www.error != null)
                {
                    Debug.Log("Error making request. Either your internet is down...or Cortex is down. Fingers crossed its the internet :)");
                    onError?.Invoke(www.error);
                    yield break;
                }

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
                settingUp = NetworkSettings.currentAddress == null || NetworkSettings.currentAddress == "" || MatryxPlatform.address == null || MatryxToken.address == null || MatryxCommit.address == null || MatryxTournament.ABI == null;
            }

            //var content = "\"{\"Items\":[\"{\"rangeKeys\":[\"t\",\"u\",\"v\",\"w\"],\"rangePairs\":[{\"min\":{\"exclusive\":false,\"rawText\":\"0\"},\"max\":{\"exclusive\":false,\"rawText\":\"0\"}},{\"min\":{\"exclusive\":false,\"rawText\":\"0\"},\"max\":{\"exclusive\":false,\"rawText\":\"pi\"}},{\"min\":{\"exclusive\":false,\"rawText\":\"0\"},\"max\":{\"exclusive\":false,\"rawText\":\"2*pi\"}},{\"min\":{\"exclusive\":false,\"rawText\":\"0\"},\"max\":{\"exclusive\":false,\"rawText\":\"0\"}}],\"ExpressionKeys\":[\"X\",\"Y\",\"Z\"],\"ExpressionValues\":[\"(2*cos(u)*cos(v))^3\",\"(2*sin(u)*cos(v))^3\",\"(2*sin(v))^3\"]}\"]}\"";
            //var bytesContent = Encoding.ASCII.GetBytes(content);
            //var multiHash = Utils.GetMultiHash(bytesContent);
            //Debug.Log("hash of content: " + multiHash);

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
        }

        private void ProcessSubmissions(object results)
        {
            var submissions = results as List<MatryxSubmission>;
            Debug.Log(submissions[0].title);
        }
    }
}