using UnityEngine;
using UnityEngine.Networking;

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
using System.Runtime.Serialization.Formatters.Binary;
using System.Linq;

namespace MatryxJsonRpc
{

    public class Request : MonoBehaviour
    {
        private static Serializer serializer = new Serializer();
        private static string explorerEndpt = "http://13.57.163.24";
        //private static string explorerEndpt = "http://13.56.237.75/";
        private static string latestPlatformInfoEndpt = explorerEndpt+"/platform/getLatestInfo";
        private static string latestTokenInfoEndpt = explorerEndpt + "/token/getLatestInfo";
        private static string allTournamentsEndpt = "/tournaments";
        private static string latestTournamentAbiEndpt = explorerEndpt + allTournamentsEndpt + "/getLatestAbi";
        private static string sharedUrl = explorerEndpt + "/tempAPI";
        private static string tournamentDetailByAddressEndpt = "/tournaments/address/";
        private static string tournamentDetailByIdEndpt = "/tournaments/";
        private static string submissionDetailByAddressEndpt = "/submissions/address/";
		private static string submissionUploadEndpt = explorerEndpt + "/ipfs/upload/";

        private static string roundDetailEndpt = sharedUrl + "/rounds/id/";

        // Contract info
        private static string mtxNode = "http://localhost:8545";
        private static string customRPCNode = "http://customrpc.matryx.ai:8545";
        private static string platformAddress;
        private static Contract platformContract;
        private static Contract tokenContract;
        private static Contract tournamentContract;
        private static string tournamentAbi;

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
            var function = platformContract.GetFunction("tournamentByIndex");
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
            using (WWW www = new WWW(explorerEndpt + allTournamentsEndpt))
            {
                yield return www;
                // Debug.Log(www.text);
                var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
                var tournamentList = jsonObj["data"] as List<object>;
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

        [FunctionOutput]
        private class EntryFeeDTO
        {
            [Parameter("uint256", 1)]
            public long entryFee { get; set; }
        }

        private static IEnumerator CoroutineListSumbissions(RoutineContext context)
        {
            // Prepare
            var function = platformContract.GetFunction("submissionByIndex");
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
            var tournamentAddress = (string)param[0];
            var page = (long)param[1];
            var offset = page * 10;
            var url = explorerEndpt + tournamentDetailByIdEndpt + tournamentAddress;
            using (WWW www = new WWW(url))
            {
                yield return www;
                // Debug.Log(www.text);
                var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
				if (jsonObj["error"] != null)
				{
					context.done(submissions);
				}
				else
				{
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
							submission.address = tournamentAddress + ":" + sub["submissionAddress"] as string;
							submission.title = sub["submissionTitle"] as string;
							submission.address += submission.title;
							submission.author = sub["authorName"] as string;
							submissions.Add(submission);
						}
						catch (System.ArgumentOutOfRangeException e) { break; }
						catch (Exception e) { Debug.Log(e); }
					}

					Debug.Log("Fetched submissions: " + submissions.Count);
					context.done(submissions);
				}
			}
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
            var function = tournamentContract.GetFunction("submissionByAddress");
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
            var url = explorerEndpt + submissionDetailByAddressEndpt + submissionAddress;
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
            // Get accounts
            var requestAccounts = new EthAccountsUnityRequest(mtxNode);
            yield return requestAccounts.SendRequest();
            var resultsAccounts = requestAccounts.Result;
            var usedAccount = resultsAccounts[0];

            // Try to create a peer.
            var createPeerFunction = platformContract.GetFunction("createPeer");
            object[] createPeerParams = { };
            var createPeerInput = createPeerFunction.CreateTransactionInput(usedAccount, createPeerParams);
            createPeerInput.Gas = new HexBigInteger(3000000);
            var createPeerCall = new EthSendTransactionUnityRequest(mtxNode);
            yield return createPeerCall.SendRequest(createPeerInput);
            try
            {
                var resultsTransaction = createPeerCall.Result;
            }
            catch (Exception e)
            {
                // Error
                Debug.Log("Could not check peer status");
                //Debug.Log(e);
                context.done(null);
            }

            // tournament.entryFee();
            var submission = (Submission)((object[])context.param)[0];
            tournamentContract = new Contract(null, tournamentAbi, submission.tournamentAddress);
            var entryFeeFunction = tournamentContract.GetFunction("entryFee");
            var entryFeeInput = entryFeeFunction.CreateCallInput(new object[0]);
            entryFeeInput.Gas = new HexBigInteger(300000);
            var entryFeeCall = new EthCallUnityRequest(mtxNode);
            yield return entryFeeCall.SendRequest(entryFeeInput, BlockParameter.CreateLatest());
            EntryFeeDTO entryFee = new EntryFeeDTO();
            try
            {
                entryFee = entryFeeFunction.DecodeDTOTypeOutput<EntryFeeDTO>(entryFeeCall.Result);
            }
            catch (Exception e)
            {
                Debug.Log("Could not get tournament entry fee.");
                context.done(null);
            }

            // token.approve(tournament.address, tournament.entryFee)
            if (entryFee.entryFee > 0)
            {

                var tokenApproveFunction = tokenContract.GetFunction("approve");
                object[] tokenClearApproveParams = { submission.tournamentAddress, 0 };
                object[] tokenApproveParams = { submission.tournamentAddress, entryFee.entryFee };
                var tokenClearApproveInput = tokenApproveFunction.CreateTransactionInput(usedAccount, tokenClearApproveParams);
                var tokenApproveInput = tokenApproveFunction.CreateTransactionInput(usedAccount, tokenApproveParams);
                tokenClearApproveInput.Gas = new HexBigInteger(3000000);
                tokenApproveInput.Gas = new HexBigInteger(3000000);
                var tokenClearApproveTransaction = new EthSendTransactionUnityRequest(mtxNode);
                var tokenApproveTransaction = new EthSendTransactionUnityRequest(mtxNode);
                yield return tokenClearApproveTransaction.SendRequest(tokenClearApproveInput);
                try
                {
                    var tokenClearApprovalResult = tokenClearApproveTransaction.Result;
                }
                catch (Exception e)
                {
                    Debug.Log("Could not approve tournament to withdraw entry fee.");
                    context.done(null);
                }

                yield return tokenApproveTransaction.SendRequest(tokenApproveInput);
                try
                {
                    var tokenApprovalTransactionResult = tokenApproveTransaction.Result;
                }
                catch (Exception e)
                {
                    Debug.Log("Could not approve tournament to withdraw entry fee.");
                    context.done(null);
                }
            }

            //platform.enterTournament(tournament.address)
            var enterTournamentFunction = platformContract.GetFunction("enterTournament");
            object[] enterTournamentParams = { submission.tournamentAddress };
            var enterTournamentInput = enterTournamentFunction.CreateTransactionInput(usedAccount, enterTournamentParams);
            enterTournamentInput.Gas = new HexBigInteger(3300000);
            var enterTournamentTransaction = new EthSendTransactionUnityRequest(mtxNode);
            yield return enterTournamentTransaction.SendRequest(enterTournamentInput);
            try
            {
                var enterTournamentTransactionResult = enterTournamentTransaction.Result;
            }
            catch (Exception e)
            {
                Debug.Log("Could not enter tournament.");
                context.done(null);
            }

            // Fix the below code
            // Prepare create submission
            var createSubmissionFunction = tournamentContract.GetFunction("createSubmission");
            // Parse routine params
            var tournamentAddress = (submission.tournamentAddress);
            var title = submission.title;

            WWWForm form = new WWWForm();
            form.AddBinaryData("description", Encoding.ASCII.GetBytes("hello world"));
            UnityWebRequest ipfsRequest = UnityWebRequest.Post(submissionUploadEndpt, form);
            yield return ipfsRequest.Send();
            print("request completed with code: " + ipfsRequest.responseCode);
            if (ipfsRequest.isError)
            {
                print("Error: " + ipfsRequest.error);
            }
            else
            {
                print("Request Response: " + ipfsRequest.downloadHandler.text);
            }

            var response = serializer.Deserialize<object>(ipfsRequest.downloadHandler.data) as Dictionary<string, object>;
            var folderHash = Encoding.UTF8.GetBytes((string)response["folderHash"]);
            string[] contributors = { };
            int[] contributorDistribution = { };
            string[] references = { };

            if (submission.references != string.Empty)
            {
                references = submission.references.Split(',');
            }
            if (submission.contributors != string.Empty)
            {
                contributors = submission.contributors.Split(',');
                contributorDistribution = Enumerable.Range(0, contributors.Length).Select(x => 1).ToArray();
            }

            // Make input
            object[] inputParams = { title, usedAccount, folderHash, contributors, contributorDistribution, references };
            var transactionInput = createSubmissionFunction.CreateTransactionInput(usedAccount, inputParams);
            transactionInput.Gas = new HexBigInteger(3000000);
            transactionInput.GasPrice = new HexBigInteger(20);
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

        private static ulong ParseHexString(string hexNumber)
        {
            hexNumber = hexNumber.Replace("0x", string.Empty);
            ulong parsed = ulong.Parse(hexNumber, System.Globalization.NumberStyles.AllowHexSpecifier);
            return parsed;
        }

        private static IEnumerator SimpleCall(EthCallUnityRequest request, CallInput input)
        {
            var block = BlockParameter.CreateLatest();
            var waiting = request.SendRequest(input, block);
            return waiting;
        }

        IEnumerator InitRoutine()
        {
            // instantiate platform contract
            using (WWW www = new WWW(latestPlatformInfoEndpt))
            {
                yield return www;
                var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;

                var platformAbi = Encoding.UTF8.GetString(serializer.Serialize(jsonObj["abi"]));
                platformAddress = jsonObj["address"] as string;
                platformContract = new Contract(null, platformAbi, platformAddress);
            }

            // instantiate token contract
            using (WWW www2 = new WWW(latestTokenInfoEndpt))
            {
                yield return www2;
                var tokenJsonObj = serializer.Deserialize<object>(www2.bytes) as Dictionary<string, object>;
                var tokenAddress = tokenJsonObj["address"] as string;

                var tokenAbi = Encoding.UTF8.GetString(serializer.Serialize(tokenJsonObj["abi"]));
                tokenContract = new Contract(null, tokenAbi, tokenAddress);
            }

            // instantiate tournament contract (without address)
            using (WWW www3 = new WWW(latestTournamentAbiEndpt))
            {
                yield return www3;
                var tournyJsonObj = serializer.Deserialize<object>(www3.bytes) as Dictionary<string, object>;
                tournamentAbi = Encoding.UTF8.GetString(serializer.Serialize(tournyJsonObj["abi"]));
                //var tournamentAbi = Encoding.UTF8.GetString(serializer.Serialize(tournyJsonObj["abi"]));
            }

            // Get accounts
            var requestAccounts = new EthAccountsUnityRequest(mtxNode);
            yield return requestAccounts.SendRequest();
            var resultsAccounts = requestAccounts.Result;
			if(resultsAccounts != null && resultsAccounts[0] != null)
			{
                StatisticsTracking.InstantEvent("Matryx Init", "Eth Node", new Dictionary<string, object>(){
                    {"Node Found", true},
                });

                var usedAccount = resultsAccounts[0];
				Debug.Log("Used account:" + usedAccount);
                var function = platformContract.GetFunction("prepareBalance");
                var transactionInput = function.CreateTransactionInput(usedAccount, (long)42);
                transactionInput.Gas = new HexBigInteger(3000000);
                // Do request
                var requestTransaction = new EthSendTransactionUnityRequest(customRPCNode);
                yield return requestTransaction.SendRequest(transactionInput);
                var resultsTransaction = requestTransaction.Result;
            }
            else
            {
                StatisticsTracking.InstantEvent("Matryx Init", "Eth Node", new Dictionary<string, object>(){
                    {"Node Found", false},
                });
            }
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