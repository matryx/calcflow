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

namespace MatryxJsonRpc
{

    public class Request : MonoBehaviour
    {

        // Contract info
        private static string mtxNode = "http://localhost:8545";
        private static string mtxContractAddr = "0x6734c8d4f6c377007d65483b60ce0f7d4bfe7101";
        private static Contract mtxContract;

        // Public api
        public delegate void ResultDelegate(object obj);

        // LIST TOURNAMENT
        public static void RunListTournaments(long page, ResultDelegate callback)
        {
            // Schedule query
            queue(CoroutineListTournaments(new RoutineContext(new object[] { page }, callback)));
        }

        [FunctionOutput]
        private class TournamentDTO
        {
            [Parameter ("uint256", "id", 1)]
            public BigInteger id { get; set; }
            [Parameter ("string", "title", 2)]
            public string title { get; set; }
            [Parameter ("string", "description", 3)]
            public string description { get; set; }
            [Parameter ("uint256", "bounty", 4)]
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
                    break;
                }
            }
            Debug.Log("Fetched tournaments: " + tournaments.Count);
            // Done
            context.done(tournaments);
        }

        // LIST SUMBISSIONS
        public static void RunListSubmissions(string tournamentAddress, long page, ResultDelegate callback)
        {
            // Schedule query
            queue(CoroutineListSumbissions(new RoutineContext(new object[] { tournamentAddress, page }, callback)));
        }

        [FunctionOutput]
        private class SubmissionDTO
        {
            [Parameter ("uint256", "id", 1)]
            public BigInteger id { get; set; }
            [Parameter ("string", "title", 2)]
            public string title { get; set; }
            [Parameter ("string", "body", 3)]
            public string body { get; set; }
            [Parameter ("string", "references", 4)]
            public string references { get; set; }
            [Parameter ("string", "contributors", 5)]
            public string contributors { get; set; }
            [Parameter ("address", "author", 6)]
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
                    Debug.Log(e);
                    break;
                }
            }
            Debug.Log("Fetched submissions: " + submissions.Count);
            // Done
            context.done(submissions);
        }

        // DETAIL SUBMISSION
        public static void RunDetailSubmission(string addresses, ResultDelegate callback)
        {
            var parts = addresses.Split(':');
            var tournamentAddress = parts[0];
            var submissionAddress = parts[1];
            queue(CoroutineDetailSubmission(new RoutineContext(new object[] { tournamentAddress, submissionAddress }, callback)));
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
                Debug.Log(e);
                context.done(null);
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
            var references = submission.references + "";
            var contributors = submission.contributors + "";
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
                Debug.Log(e);
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
            var usedAccount = resultsAccounts[0];
            Debug.Log("Used account:" + resultsAccounts);
            var function = mtxContract.GetFunction("prepareBalance");
            var transactionInput = function.CreateTransactionInput(usedAccount, (long)42);
            // Do request
            var requestTransaction = new EthSendTransactionUnityRequest(mtxNode);
            yield return requestTransaction.SendRequest(transactionInput);
            var resultsTransaction = requestTransaction.Result;
        }

        void Start()
        {
            /*
            StartCoroutine(InitRoutine());
            Debug.Log("Balance Of start");
            RunBalanceOf("0x31a8f8c08accb2923049d438f13295d5717b387b", delegate (object result)
            {
                Debug.Log("BalanceOfResult");
                Debug.Log(result);
            });
            */
            Debug.Log("RunListTournaments START");
            RunListTournaments(0, delegate (object result)
            {
                Debug.Log("RunListTournaments RESULT");
                Debug.Log(result);
            });
            /*
            Debug.Log("RunListSubmissions START");
            RunListSubmissions("1", 0, delegate (object result)
            {
                Debug.Log("RunListSubmissions RESULT");
                Debug.Log(result);
            });
            Debug.Log("RunDetailSubmission START");
            RunDetailSubmission("1:1", delegate (object result)
            {
                Debug.Log("RunDetailSubmission RESULT");
                Debug.Log(result);
            });
            */
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
            lock(_queue)
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