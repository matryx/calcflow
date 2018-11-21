using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

using Nanome.Maths.Serializers.JsonSerializer;
using UnityEngine.Networking;
using Nanome.Core;
using System.Numerics;

namespace Matryx
{
    public class MatryxExplorer : MonoBehaviour
    {
        private static Serializer serializer = new Serializer();
        private static string explorerURL = "https://explorer-dev.matryx.ai"; //"https://explorer.matryx.ai";
        private static string platformInfoURL = explorerURL + "/platform/getInfo";
        private static string tokenInfoURL = explorerURL + "/token/getInfo";
        private static string tournamentsURL = explorerURL + "/tournaments";
        private static string tournamentAbiURL = tournamentsURL + "/getAbi";

        private static string tournamentURL = "/tournaments/address/";
        private static string roundURL = "/round/address";
        private static string submissionURL = "/submissions/address/";

        private static string uploadURL = explorerURL + "/ipfs/upload/";

        // Public api
        public delegate void ResultDelegate(object obj);

        // LIST TOURNAMENT
        public static void RunFetchTournaments(long page, ResultDelegate callback)
        {
            // Schedule query
            // queue(CoroutineListTournaments(new RoutineContext(new object[] { page }, callback)));
            queue(FetchTournaments(new RoutineContext(new object[] { page }, callback)));
        }

        private static IEnumerator FetchTournaments(RoutineContext context)
        {
            var tournaments = new List<MatryxTournament>();
            var param = (object[])context.param;
            var page = (long)param[0];
            var offset = page * 10;
            using (WWW www = new WWW(tournamentsURL))
            {
                yield return www;
                // Debug.Log(www.text);
                var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                var tournamentList = jsonObj["tournaments"] as List<object>;
                for (int i = 0; i < 10; i++)
                {
                    try
                    {
                        // var tourna = tournamentList[i + (int)offset] as Dictionary<string, object>;
                        var tourna = tournamentList[i] as Dictionary<string, object>;
                        var tournament = new MatryxTournament(tourna["address"] as string);
                        tournament.title = tourna["title"] as string;
                        tournament.descriptionHash = tourna["description"] as string;
                        tournament.bounty = (long)Convert.ToDouble(tourna["bounty"]);
                        tournaments.Add(tournament);
                    }
                    catch (System.ArgumentOutOfRangeException e) { break; }
                    catch (Exception e) { Debug.Log(e); }
                }
                Debug.Log("Fetched tournaments: " + tournaments.Count);
                context.done(tournaments);
            }
        }

        // LIST SUMBISSIONS
        public static void RunFetchSubmissions(string tournamentAddress, long roundNumber, long page, ResultDelegate callback)
        {
            // Schedule query
            queue(FetchSubmissions(new RoutineContext(new object[] { tournamentAddress, roundNumber, page }, callback)));
        }

        private static IEnumerator FetchSubmissions(RoutineContext context)
        {
            var submissions = new List<MatryxSubmission>();
            var param = (object[])context.param;
            var tournamentAddress = (string)param[0];
            var roundNumber = (long)param[1];
            var page = (long)param[2];
            var offset = page * 10;

            yield return null;

            //         using (WWW www = new WWW(tournamentURL + tournamentAddress))
            //         {
            //             yield return www;
            //             // Debug.Log(www.text);
            //             var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
            //	if (jsonObj["error"] != null)
            //	{
            //		context.done(submissions);
            //	}
            //	else
            //	{
            //		Debug.Log(tournamentURL + tournamentAddress + " - " + jsonObj["message"] as string);
            //		var dataObj = jsonObj["data"] as Dictionary<string, object>;
            //		var title = dataObj["tournamentTitle"] as string;
            //		var bounty = Convert.ToDouble(dataObj["mtx"] as string);
            //		var description = dataObj["tournamentDescription"] as string;
            //		var submissionList = dataObj["recentSubmissions"] as List<object>;
            //		for (int i = 0; i < 10; i++)
            //		{
            //			try
            //			{
            //				var sub = submissionList[i + (int)offset] as Dictionary<string, object>;
            //				var submission = new Submission();
            //				submission.address = tournamentAddress + ":" + sub["submissionAddress"] as string;
            //				submission.title = sub["submissionTitle"] as string;
            //				submission.address += submission.title;
            //				submission.author = sub["authorName"] as string;
            //				submissions.Add(submission);
            //			}
            //			catch (System.ArgumentOutOfRangeException e) { break; }
            //			catch (Exception e) { Debug.Log(e); }
            //		}

            //		Debug.Log("Fetched submissions: " + submissions.Count);
            //		context.done(submissions);
            //	}
            //}
        }

        // DETAIL SUBMISSION
        public static void RunDetailSubmission(string addresses, ResultDelegate callback)
        {
            var parts = addresses.Split(':');
            var tournamentAddress = parts[0];
            var submissionAddress = parts[1];
            queue(MtxExplorerDetailSubmission(new RoutineContext(new object[] { tournamentAddress, submissionAddress }, callback)));
        }

        private static IEnumerator MtxExplorerDetailSubmission(RoutineContext context)
        {
            var param = context.param as object[];
            var tournamentAddress = param[0] as string;
            var submissionAddress = param[1] as string;
            var url = explorerURL + submissionURL + submissionAddress;
            using (var www = new WWW(url))
            {
                yield return www;
                Debug.Log(www.text);
                var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                Debug.Log(url + " - " + jsonObj["message"] as string);
                var dataObj = jsonObj["data"] as Dictionary<string, object>;
                try
                {
                    var submission = new MatryxSubmission(dataObj["submissionAddress"] as string);
                    submission.tournament = new MatryxTournament(tournamentAddress);
                    submission.details.title = dataObj["title"] as string;
                    submission.info.owner = dataObj["owner"] as string;
                    submission.details.references = (dataObj["references"] as List<string>);
                    submission.details.contributors = (dataObj["contributors"] as List<string>);
                    // TODO: Fix
                    //var bodyObj = (dataObj["description"] as List<object>)[0] as Dictionary<string, object>;
                    //var jsonContent = (bodyObj["Items"] as List<object>)[0] as string;
                    //submission.body = "{ \"Items\" : [\"" + jsonContent + "\"] }";
                    context.done(submission);
                }
                catch (Exception e)
                {
                    Debug.Log("Could not read submission at:" + submissionAddress + " tournament: " + tournamentAddress);
                    Debug.Log(e);
                    context.done(null);
                }
            }
        }

        // UPLOAD SUBMISSION
        public static void RunUploadSubmission(MatryxSubmission submission, ResultDelegate callback)
        {
            // Schedule query
            queue(CoroutineUploadSubmission(new RoutineContext(new object[] { submission }, callback)));
        }

        private static IEnumerator CoroutineUploadSubmission(RoutineContext context)
        {
            yield return 4;
        }

        private static ulong ParseHexString(string hexNumber)
        {
            hexNumber = hexNumber.Replace("0x", string.Empty);
            ulong parsed = ulong.Parse(hexNumber, System.Globalization.NumberStyles.AllowHexSpecifier);
            return parsed;
        }

        public static IEnumerator InitializeMatryxGlobals()
        {
            // TODO: Get working
            Dictionary<string, string> postData = new Dictionary<string, string>();
            postData.Add("jsonrpc", "2.0");
            postData.Add("method", "eth_getBlockByNumber");
            postData.Add("params", "['latest',false]");
            postData.Add("id", "1");
            UnityWebRequest blockByNumberRequest = UnityWebRequest.Post(Network.infuraProvider, postData); //"{\"jsonrpc\":\"2.0\",\"method\":\"eth_getBlockByNumber\",\"params\": [\"0x5BAD55\",false],\"id\":1}"
            blockByNumberRequest.SetRequestHeader("Content-Type", "application/json");
            yield return blockByNumberRequest.Send();
            print("request completed with code: " + blockByNumberRequest.responseCode);
            if (blockByNumberRequest.isNetworkError)
            {
                print("Error: " + blockByNumberRequest.error);
            }
            else
            {
                print("Request Response: " + blockByNumberRequest.downloadHandler.text);
            }

            var response = serializer.Deserialize<object>(blockByNumberRequest.downloadHandler.data) as Dictionary<string, object>;

            //var blockGasEstimate = new Nethereum.JsonRpc.Client.RpcRequest(1, "eth_getBlockByNumber", "0x4380d5", false);
            //var request = new Nethereum.JsonRpc.UnityClient.EthCallUnityRequest(MatryxGlobals.infuraProvider);
            //yield return request.SendRequest(blockGasEstimate);

            //MatryxGlobals.txGas = request.Result;
        }

        public static IEnumerator InitializePlatformContract()
        {
            using (WWW www = new WWW(platformInfoURL))
            {
                yield return www;
                var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;

                var address = jsonObj["address"] as string;
                var abi = Encoding.UTF8.GetString(serializer.Serialize(jsonObj["abi"]));
                if (MatryxPlatform.address == null)
                {
                    MatryxPlatform.address = address;
                    MatryxPlatform.ABI = abi;
                    MatryxPlatform.contract = new Nethereum.Contracts.Contract(null, abi, address);
                }
            }
        }

        public static IEnumerator InitializeTokenContract()
        {
            using (WWW www = new WWW(tokenInfoURL))
            {
                yield return www;
                var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;

                if (MatryxToken.address == null)
                {
                    var address = jsonObj["address"] as string;
                    var abi = Encoding.UTF8.GetString(serializer.Serialize(jsonObj["abi"]));
                    MatryxToken.address = address;
                    MatryxToken.ABI = abi;
                    MatryxToken.contract = new Nethereum.Contracts.Contract(null, abi, address);

                    //Async main = Async.runInCoroutine(delegate (Async thread)
                    //{
                    //    return MatryxToken.approve(thread, "0xea94c9F4022DD122a19cb9f7f8cD2e00064037c7", new BigInteger(2e18));
                    //});
                    //main.onEvent("enterMatryx-success", delegate (object datas2)
                    //{
                    //    Debug.Log("Woo!");
                    //});

                    //yield return MatryxToken.approve("0xea94c9F4022DD122a19cb9f7f8cD2e00064037c7", new BigInteger(2e18));
                }
            }
        }

        public static IEnumerator InitializeTournamentContract()
        {
            // instantiate tournament contract (without address)
            using (WWW www3 = new WWW(tournamentAbiURL))
            {
                yield return www3;
                var tournyJsonObj = serializer.Deserialize<object>(www3.bytes) as Dictionary<string, object>;
                MatryxTournament.ABI = Encoding.UTF8.GetString(serializer.Serialize(tournyJsonObj["abi"]));
            }

            var tournament = new MatryxTournament("0xea94c9F4022DD122a19cb9f7f8cD2e00064037c7");
            //yield return tournament.enter();
        }

        public static IEnumerator RunTests()
        {
            while(MatryxPlatform.address == null || MatryxToken.address == null || MatryxTournament.ABI == null)
            {
                yield return null;
            }


            var tournament = new MatryxTournament("0xc1fb995A5236632b3E0abe05aFae76dD10d69e21");
            //yield return MatryxPlatform.createTournament("Tournament created within Calcflow", "math", "QmPG5E2R9d13BuXfyUerT88kVFgV1fYgABPhU6LFHJM2ho", "", new System.Numerics.BigInteger(100e18), new System.Numerics.BigInteger(2e18), BigInteger roundStart, BigInteger roundEnd, BigInteger roundReview, BigInteger roundBounty)
            yield return MatryxToken.approve(tournament.address, new System.Numerics.BigInteger(2e18));
            //yield return MatryxPlatform.hasEnteredMatryx(MatryxGlobals.account);
            //yield return MatryxPlatform.getUsers(0, 0);
            //Async main = Async.runInCoroutine(delegate (Async thread)
            //{
            //    return tournament.enter(thread);
            //});
            //main.onEvent("enterTournament-success", delegate (object data)
            //{
            //    Debug.Log("check yo laptop");
            //});
            //main.onEvent("enterTournament-failure", delegate (object data)
            //{
            //    Debug.Log("check yo laptop");
            //});

            //yield return tournament.getRounds();
            //yield return tournament.getCurrentRound();
            //yield return tournament.getSubmissionCount();
            //yield return tournament.isEntrant(Network.account);
            //yield return tournament.enter();
        }

        void Start()
        {
            queue(InitializePlatformContract());
            queue(InitializeTokenContract());
            queue(InitializeTournamentContract());
            queue(RunTests());
            //queue(InitializeMatryxGlobals());
            ///*
            Debug.Log("RunListTournaments START");
            RunFetchTournaments(0, delegate (object result)
            {
                Debug.Log("RunListTournaments RESULT");
                Debug.Log(result);
            });
            //*/
            /*
            Debug.Log("RunListSubmissions START");
            RunListSubmissions("42", 0, delegate (object result)
            {
                Debug.Log("RunListSubmissions RESULT");
                Debug.Log(result);
            });
            //*/
            /*
            Debug.Log("RunDetailSubmission START");
            RunDetailSubmission("1:1", delegate (object result)
            {
                Debug.Log("RunDetailSubmission RESULT");
                Debug.Log(result);
            });
            //*/
            /*
            Debug.Log("RunUploadSubmission START");
            var sub = new Submission();
            sub.tournamentAddress = "42";
            sub.title = "UPLOADED SUB TITLE";
            sub.body = "UPLOADED SUB BODY";
            sub.references = "UPLOADED SUB REFERENCES";
            sub.contributors = "UPLOADED SUB CONTRIBUTORS";
            RunUploadSubmission(sub, delegate (object result)
            {
                Debug.Log("RunUploadSubmission RESULT");
                Debug.Log(result);
            });
            */
        }

        // Coroutine param
        private class RoutineContext
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
        }

    }
}