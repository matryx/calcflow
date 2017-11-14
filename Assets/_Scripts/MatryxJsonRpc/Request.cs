using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;
using System.Net;
using System.Text;
using System.IO;

using Nethereum.ABI.FunctionEncoding;
using Nethereum.ABI.Model;
using Nethereum.Contracts;
using Nethereum.Hex.HexTypes;
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
        private static string mtxContractAddr = "0x0af44e2784637218dd1d32a322d44e603a8f0c6a";
        private static Contract mtxContract;

        // Public api
        public delegate void ResultDelegate(object obj);

        // LIST TOURNAMENT
        public static void RunListTournaments(long page, ResultDelegate callback)
        {
            // Schedule query
            queue(CoroutineListTournaments(new RoutineContext(new object[] { page }, callback)));
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
            var offset = page * 25;
            for (var i = 0; i < 25; i++)
            {
                // Make input
                var input = function.CreateCallInput(i + offset);
                // Request the specific tournament at the index
                var request = new EthCallUnityRequest(mtxNode);
                yield return SimpleCall(request, input);
                var parsedResults = function.DecodeInput(request.Result);
                // Read results
                var tournament = new Tournament();
                var j = 0;
                foreach (var parsedResult in parsedResults)
                {
                    Debug.Log("Tournament Parsed result: " + j);
                    Debug.Log(parsedResult);
                    j++;
                }
                tournament.title = (string)parsedResults[0].Result;
                tournament.description = (string)parsedResults[1].Result;
                tournament.bounty = (long)parsedResults[2].Result;
                tournament.address = (string)parsedResults[3].Result;
                // Add to list of tournaments
                tournaments.Add(tournament);
            }
            // Done
            context.done(tournaments);
        }

        // LIST SUMBISSIONS
        public static void RunListSubmissions(string tournamentAddress, long page, ResultDelegate callback)
        {
            // Schedule query
            queue(CoroutineListTournaments(new RoutineContext(new object[] { tournamentAddress, page }, callback)));
        }
        private static IEnumerator CoroutineListSumbissions(RoutineContext context)
        {
            // Prepare
            var function = mtxContract.GetFunction("submissionByIndex");
            var submissions = new List<object>();
            // Parse routine params
            var param = (object[])context.param;
            var tournamentAddress = (string)param[0];
            var page = (long)param[1];
            // Loop over every needed indexes
            var offset = page * 25;
            for (var i = 0; i < 25; i++)
            {
                // Make input
                var input = function.CreateCallInput(tournamentAddress, i + offset);
                // Request the specific submission within tournament address at index
                var request = new EthCallUnityRequest(mtxNode);
                yield return SimpleCall(request, input);
                var parsedResults = function.DecodeInput(request.Result);
                // Read results
                var submission = new Submission();
                var j = 0;
                foreach (var parsedResult in parsedResults)
                {
                    Debug.Log("Submission Parsed result: " + j);
                    Debug.Log(parsedResult);
                    j++;
                }
                submission.title = (string)parsedResults[0].Result;
                submission.body = (string)parsedResults[1].Result;
                submission.author = (string)parsedResults[2].Result;
                submission.address = (string)parsedResults[3].Result;
                submission.tournamentAddress = (string)parsedResults[4].Result;
                submission.references = (string)parsedResults[5].Result;
                submission.contributors = (string)parsedResults[6].Result;
                // Add to list of submission
                submissions.Add(submission);
            }
            // Done
            context.done(submissions);
        }

        // DETAIL SUBMISSION
        public static void RunDetailSubmission(string submissionAddress, ResultDelegate callback)
        {
            // Schedule query
            queue(CoroutineListTournaments(new RoutineContext(new object[] { submissionAddress }, callback)));
        }
        private static IEnumerator CoroutineDetailSubmission(RoutineContext context)
        {
            // Prepare
            var function = mtxContract.GetFunction("submissionByAddress");
            // Parse routine params
            var param = (object[])context.param;
            var submissionAddress = (string)param[0];
            // Make input
            var input = function.CreateCallInput(submissionAddress);
            // Request the specific submission at address
            var request = new EthCallUnityRequest(mtxNode);
            yield return SimpleCall(request, input);
            var parsedResults = function.DecodeInput(request.Result);
            // Read results
            var submission = new Submission();
            var j = 0;
            foreach (var parsedResult in parsedResults)
            {
                Debug.Log("Submission Parsed result: " + j);
                Debug.Log(parsedResult);
            }
            submission.title = (string)parsedResults[0].Result;
            submission.body = (string)parsedResults[1].Result;
            submission.author = (string)parsedResults[2].Result;
            submission.address = (string)parsedResults[3].Result;
            submission.tournamentAddress = (string)parsedResults[4].Result;
            submission.references = (string)parsedResults[5].Result;
            submission.contributors = (string)parsedResults[6].Result;
            // Done
            context.done(submission); // TODO
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
            // Parse routine params
            var param = (object[])context.param;
            var submission = (Submission)param[0];
            var tournamentAddress = submission.tournamentAddress;
            var title = submission.title;
            var body = submission.body;
            var references = submission.references;
            var contributors = submission.contributors;
            // Make input
            var functionInput = function.SendTransactionAsync("", tournamentAddress, title, body, references, contributors);
            // TODO
            context.done(null);
            yield return null;
        }

        /*
        public static void RunBalanceOf(string address, ResultDelegate callback)
        {
            queue(CoroutineBalanceOf(new RoutineContext(address, callback)));
        }
        private static IEnumerator CoroutineBalanceOf(RoutineContext context)
        {
            // Needed params
            var request = new EthCallUnityRequest(mtxNode);
            var address = (string)context.param;
            var function = mtxContract.GetFunction("balanceOf");
            // Run and wait for result
            yield return SimpleCall(request, function.CreateCallInput(address));
            // Parse result
            var parsedResult = function.DecodeSimpleTypeOutput<long>(request.Result);
            context.done(parsedResult);
        }
        */

        private static IEnumerator SimpleCall(EthCallUnityRequest request, CallInput input)
        {
            var block = BlockParameter.CreateLatest();
            var waiting = request.SendRequest(input, block);
            return waiting;
        }

        private static IEnumerator SimpleTransaction(TransactionSignedUnityRequest request, TransactionInput input)
        {
            var waiting = request.SignAndSendTransaction(input);
            return waiting;
        }

        // Read basic infos about contracts
        void Awake()
        {
            var abi = (Resources.Load("mtxAbi") as TextAsset).text;
            mtxContract = new Contract(null, abi, mtxContractAddr);
        }

        void Start()
        {
            /*
            Debug.Log("Balance Of start");
            RunBalanceOf("0x31a8f8c08accb2923049d438f13295d5717b387b", delegate (object result)
            {
                Debug.Log("BalanceOfResult");
                Debug.Log(result);
            });
            */
            RunListTournaments(0, delegate (object result)
            {

            });
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