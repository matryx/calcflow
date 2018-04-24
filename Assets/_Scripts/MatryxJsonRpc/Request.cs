using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;
using System.Net;
using System.Text;
using System.IO;
using System.Numerics;

using Nethereum.ABI.Encoders;
using Nethereum.ABI.FunctionEncoding.Attributes;
using Nethereum.ABI.FunctionEncoding;
using Nethereum.ABI.Model;
using Nethereum.Contracts;
using Nethereum.Hex.HexTypes;
using Nethereum.Hex.HexConvertors.Extensions;
using Nethereum.JsonRpc.Client;
using Nethereum.JsonRpc.UnityClient;
using Nethereum.Util;
using Nethereum.RPC.Eth.DTOs;
using Nethereum.RPC.Eth.Transactions;
using Nethereum.Signer;

using Nanome.Maths.Serializers.JsonSerializer;

using Calcflow.UserStatistics;

namespace MatryxJsonRpc
{

    public class Request : MonoBehaviour
    {
        private static Serializer serializer = new Serializer();
        private static string sharedUrl = "http://54.183.167.220/tempAPI";
        private static string allTournamentsEndpt = "/tournaments";
        private static string tournamentDetailByAddressEndpt = "/tournaments/address/";
        private static string tournamentDetailByIdEndpt = "/tournaments/id/";
        private static string submissionDetailByAddressEndpt = "/submissions/address/";

        private static string roundDetailEndpt = "http://54.183.167.220/tempAPI/rounds/id/";

        // Contract info
        private static string mtxNode = "http://localhost:8545";
        private static string mtxContractAddr = "0x7c4970b887cfa95062ead0708267009dcd564017";
        private static Contract mtxContract;

        // Public api
        public delegate void ResultDelegate(object obj);

        // LIST TOURNAMENT
        public static void RunListTournaments(long page, ResultDelegate callback)
        {
            // Schedule query
            // queue(CoroutineListTournaments(new RoutineContext(new object[] { page }, callback)));
            queue(MtxExplorerListTournaments(new RoutineContext(new object[] { page }, callback)));
        }

        [FunctionOutput]
        private class TournamentDTO
        {
            [Parameter("uint256", "id", 1)]
            public BigInteger id { get; set; }
            [Parameter("string", "title", 2)]
            public string title { get; set; }
            [Parameter("string", "description", 3)]
            public string description { get; set; }
            [Parameter("uint256", "bounty", 4)]
            public BigInteger bounty { get; set; }
        }

        private static IEnumerator CoroutineListTournaments(RoutineContext context)
        {
            // Prepare
            var function = mtxContract.GetFunction("tournamentByIndex");
            var tournaments = new List<Tournament>();
            // Parse routine params
            var param = (object[])context.param;
            var page = (long)param[0];
            // Loop over every needed indexes
            var offset = page * 10;
            Debug.Log("Loading tournaments at: " + offset);
            for (var i = 0; i < 10; i++)
            {
                // Make input
                var input = function.CreateCallInput(new BigInteger(i + offset));
                // Request the specific tournament at the index
                var request = new EthCallUnityRequest(mtxNode);
                yield return SimpleCall(request, input);
                try
                {
                    var parsedResults = function.DecodeDTOTypeOutput<TournamentDTO>(request.Result);
                    // Read results
                    var tournament = new Tournament();
                    tournament.address = parsedResults.id.ToString();
                    tournament.title = parsedResults.title;
                    tournament.description = parsedResults.description;
                    tournament.bounty = (long)parsedResults.bounty;
                    // Add to list of tournaments
                    tournaments.Add(tournament);
                }
                catch (Exception e)
                {
                    Debug.Log("Could not read tournament at index:" + (offset + i));
                    Debug.Log(e);
                    //break;
                }
            }
            Debug.Log("Fetched tournaments: " + tournaments.Count);
            // Done
            context.done(tournaments);
        }

        private static IEnumerator MtxExplorerListTournaments(RoutineContext context)
        {
            var tournaments = new List<Tournament>();
            var param = (object[])context.param;
            var page = (long)param[0];
            var offset = page * 10;
            using (WWW www = new WWW(sharedUrl + allTournamentsEndpt))
            {
                yield return www;
                // Debug.Log(www.text);
                var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                Debug.Log(allTournamentsEndpt + " - " + jsonObj["message"] as string);
                var dataObj = jsonObj["data"] as Dictionary<string, object>;
                var tournamentList = dataObj["tournaments"] as List<object>;
                for (int i = 0; i < 10; i++)
                {
                    try
                    {
                        var tourna = tournamentList[i + (int)offset] as Dictionary<string, object>;
                        var tournament = new Tournament();
                        tournament.title = tourna["tournamentTitle"] as string;
                        tournament.description = tourna["tournamentDescription"] as string;
                        tournament.bounty = (long)Convert.ToDouble(tourna["mtx"]);
                        tournament.address = tourna["address"] as string;
                        tournament.id = Convert.ToInt64(tourna["tournamentID"]);
                        // tournament.address = tournament.id.ToString();
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
        public static void RunListSubmissions(string tournamentAddress, long page, ResultDelegate callback)
        {
            // Schedule query
            // queue(CoroutineListSumbissions(new RoutineContext(new object[] { tournamentAddress, page }, callback)));
            queue(MtxExplorerListSubmissions(new RoutineContext(new object[] { tournamentAddress, page }, callback)));
        }

        [FunctionOutput]
        private class SubmissionDTO
        {
            [Parameter("uint256", "id", 1)]
            public long id { get; set; }
            [Parameter("string", "title", 2)]
            public string title { get; set; }
            [Parameter("string", "body", 3)]
            public string body { get; set; }
            [Parameter("string", "references", 4)]
            public string references { get; set; }
            [Parameter("string", "contributors", 5)]
            public string contributors { get; set; }
            [Parameter("address", "author", 6)]
            public string author { get; set; }
        }

        private static IEnumerator CoroutineListSumbissions(RoutineContext context)
        {
            // Prepare
            var function = mtxContract.GetFunction("submissionByIndex");
            var submissions = new List<Submission>();
            // Parse routine params
            var param = (object[])context.param;
            var tournamentAddress = (string)param[0];
            var page = (long)param[1];
            // Loop over every needed indexes
            var offset = page * 10;
            Debug.Log("Loading submissions at: " + offset + " in tournament: " + tournamentAddress);
            for (var i = 0; i < 10; i++)
            {
                // Make input
                var input = function.CreateCallInput(new BigInteger(Convert.ToInt64(tournamentAddress)), i + offset);
                // Request the specific submission within tournament address at index
                var request = new EthCallUnityRequest(mtxNode);
                yield return SimpleCall(request, input);
                // Read results
                try
                {
                    var parsedResults = function.DecodeDTOTypeOutput<SubmissionDTO>(request.Result);
                    // Read results
                    var submission = new Submission();
                    submission.tournamentAddress = tournamentAddress;
                    submission.address = tournamentAddress + ":" + parsedResults.id.ToString();
                    submission.title = parsedResults.title;
                    submission.body = parsedResults.body;
                    submission.references = parsedResults.references;
                    submission.contributors = parsedResults.contributors;
                    submission.author = parsedResults.author;
                    // Add to list of submissions
                    submissions.Add(submission);
                }
                catch (Exception e)
                {
                    Debug.Log("Could not read submission at index: " + (offset + i));
                    //Debug.Log(e);
                    //break;
                }
            }
            Debug.Log("Fetched submissions: " + submissions.Count);
            // Done
            context.done(submissions);
        }

        private static IEnumerator MtxExplorerListSubmissions(RoutineContext context)
        {
            var submissions = new List<Submission>();
            var param = (object[])context.param;
            var uniqueId = (string)param[0];
            var page = (long)param[1];
            var offset = page * 10;
            var url = sharedUrl + tournamentDetailByIdEndpt + uniqueId;
            using (WWW www = new WWW(url))
            {
                yield return www;
                // Debug.Log(www.text);
                var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                Debug.Log(url + " - " + jsonObj["message"] as string);
                var dataObj = jsonObj["data"] as Dictionary<string, object>;
                var title = dataObj["tournamentTitle"] as string;
                var bounty = Convert.ToDouble(dataObj["mtx"] as string);
                var description = dataObj["tournamentDescription"] as string;
                var submissionList = dataObj["recentSubmissions"] as List<object>;
                for (int i = 0; i < 10; i++)
                {
                    try
                    {
                        var sub = submissionList[i + (int)offset] as Dictionary<string, object>;
                        var submission = new Submission();
                        submission.address = uniqueId + ":" + sub["submissionAddress"] as string;
                        submission.title = sub["submissionTitle"] as string;
                        submission.address += submission.title;
                        submission.author = sub["authorName"] as string;
                        submissions.Add(submission);
                    }
                    catch (System.ArgumentOutOfRangeException e) { break; }
                    catch (Exception e) { Debug.Log(e); }
                }
            }
            Debug.Log("Fetched submissions: " + submissions.Count);
            context.done(submissions);
        }

        // DETAIL SUBMISSION
        public static void RunDetailSubmission(string addresses, ResultDelegate callback)
        {
            var parts = addresses.Split(':');
            var tournamentAddress = parts[0];
            var submissionAddress = parts[1];
            queue(MtxExplorerDetailSubmission(new RoutineContext(new object[] { tournamentAddress, submissionAddress }, callback)));
        }

        private static IEnumerator CoroutineDetailSubmission(RoutineContext context)
        {
            // Prepare
            var function = mtxContract.GetFunction("submissionByAddress");
            // Parse routine params
            var param = (object[])context.param;
            var tournamentAddress = (string)param[0];
            var submissionAddress = (string)param[1];
            // Make input
            var input = function.CreateCallInput(Convert.ToInt64(tournamentAddress), Convert.ToInt64(submissionAddress));
            // Request the specific submission at address
            var request = new EthCallUnityRequest(mtxNode);
            yield return SimpleCall(request, input);
            // Read results
            try
            {
                var parsedResults = function.DecodeDTOTypeOutput<SubmissionDTO>(request.Result);
                // Read results
                var submission = new Submission();
                submission.tournamentAddress = tournamentAddress;
                submission.address = tournamentAddress + ":" + parsedResults.id.ToString();
                submission.title = parsedResults.title;
                submission.body = parsedResults.body;
                submission.references = parsedResults.references;
                submission.contributors = parsedResults.contributors;
                submission.author = parsedResults.author;
                // Done
                context.done(submission);
            }
            catch (Exception e)
            {
                Debug.Log("Could not read submission at:" + submissionAddress + " tournament: " + tournamentAddress);
                //Debug.Log(e);
                context.done(null);
            }
        }

        private static IEnumerator MtxExplorerDetailSubmission(RoutineContext context)
        {
            var param = context.param as object[];
            var tournamentAddress = param[0] as string;
            var submissionAddress = param[1] as string;
            var url = sharedUrl + submissionDetailByAddressEndpt + submissionAddress;
            using (var www = new WWW(url))
            {
                yield return www;
                Debug.Log(www.text);
                var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                Debug.Log(url + " - " + jsonObj["message"] as string);
                var dataObj = jsonObj["data"] as Dictionary<string, object>;
                try
                {
                    var submission = new Submission();
                    submission.tournamentAddress = tournamentAddress;
                    submission.title = dataObj["submissionTitle"] as string;
                    submission.address = tournamentAddress + ":" + dataObj["submissionAddress"] as string;
                    submission.author = dataObj["submissionAuthor"] as string;
                    submission.references = (dataObj["submissionReferences"] as List<object>)[0] as string;
                    submission.contributors = (dataObj["submissionCollaborators"] as List<object>)[0] as string;
                    var bodyObj = (dataObj["submissionJson"] as List<object>)[0] as Dictionary<string, object>;
                    var jsonContent = (bodyObj["Items"] as List<object>)[0] as string;
                    submission.body = "{ \"Items\" : [\"" + jsonContent + "\"] }";
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
        public static void RunUploadSubmission(Submission submission, ResultDelegate callback)
        {
            // Schedule query
            queue(CoroutineUploadSubmission(new RoutineContext(new object[] { submission }, callback)));
        }

        private static IEnumerator CoroutineUploadSubmission(RoutineContext context)
        {
            // Prepare
            var function = mtxContract.GetFunction("createSubmission");
            // Get accounts
            var requestAccounts = new EthAccountsUnityRequest(mtxNode);
            yield return requestAccounts.SendRequest();
            var resultsAccounts = requestAccounts.Result;
            var usedAccount = resultsAccounts[0];
            // Parse routine params
            var param = (object[])context.param;
            var submission = (Submission)param[0];
            var tournamentAddress = Convert.ToInt64(submission.tournamentAddress);
            var title = submission.title;
            var body = submission.body;
            var references = submission.references + "\n";
            var contributors = submission.contributors + "\n";
            // Make input
            var transactionInput = function.CreateTransactionInput(usedAccount, tournamentAddress, title, body, references, contributors);
            transactionInput.Gas = new HexBigInteger(3000000);
            // Do request
            var requestTransaction = new EthSendTransactionUnityRequest(mtxNode);
            yield return requestTransaction.SendRequest(transactionInput);
            // Results
            try
            {
                var resultsTransaction = requestTransaction.Result;
                // Success
                context.done(resultsTransaction);
            }
            catch (Exception e)
            {
                // Error
                Debug.Log("Could not submit submission");
                //Debug.Log(e);
                context.done(null);
            }
        }

        private static IEnumerator SimpleCall(EthCallUnityRequest request, CallInput input)
        {
            var block = BlockParameter.CreateLatest();
            var waiting = request.SendRequest(input, block);
            return waiting;
        }

        // Read basic infos about contracts
        void Awake()
        {
            var abi = (Resources.Load("mtxAbi") as TextAsset).text;
            mtxContract = new Contract(null, abi, mtxContractAddr);
        }

        IEnumerator InitRoutine()
        {
            // Get accounts
            var requestAccounts = new EthAccountsUnityRequest(mtxNode);
            yield return requestAccounts.SendRequest();
            var resultsAccounts = requestAccounts.Result;
            try
            {
                var usedAccountLol = resultsAccounts[0];
                StatisticsTracking.InstantEvent("Matryx Init", "Eth Node", new Dictionary<string, object>(){
                    {"Node Found", true},
                });
            }
            catch (Exception e)
            {
                StatisticsTracking.InstantEvent("Matryx Init", "Eth Node", new Dictionary<string, object>(){
                    {"Node Found", false},
                });
            }
            var usedAccount = resultsAccounts[0];
            Debug.Log("Used account:" + usedAccount);
            var function = mtxContract.GetFunction("prepareBalance");
            var transactionInput = function.CreateTransactionInput(usedAccount, (long)42);
            transactionInput.Gas = new HexBigInteger(3000000);
            // Do request
            var requestTransaction = new EthSendTransactionUnityRequest(mtxNode);
            yield return requestTransaction.SendRequest(transactionInput);
            var resultsTransaction = requestTransaction.Result;
        }

        void Start()
        {
            StartCoroutine(InitRoutine());
            ///*
            Debug.Log("RunListTournaments START");
            RunListTournaments(0, delegate (object result)
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