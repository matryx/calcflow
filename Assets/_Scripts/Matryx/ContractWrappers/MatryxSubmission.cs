using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Nethereum.ABI.FunctionEncoding.Attributes;
using Nethereum.ABI.Model;
using Nethereum.Contracts;
using Nethereum.JsonRpc.UnityClient;
using Nanome.Core;
using System.Numerics;

namespace Matryx
{
    public class MatryxSubmission
    {
        public MatryxSubmission(string title, List<string> contributors, List<string> references, string descHash, string description=null)
        {
            this.details.title = title;
            this.details.contributors = contributors;
            this.details.references = references;
            this.details.descHash = descHash;
            this.description = description;
        }
        public MatryxSubmission(string address)
        {
            this.address = address;
        }
        public MatryxSubmission(string title, string address)
        {
            this.details.title = title;
            this.address = address;
        }

        public string address;
        public MatryxTournament tournament;
        public SubmissionInfo info;
        public SubmissionDetails details;

        public string description;
        public string file;
        public bool calcflowCompatible = false;
        public string EquationJson
        {
            get
            {
                if (!calcflowCompatible) return ""; else return file;
            }
        }

        public struct SubmissionInfo
        {
            public uint version;
            public string owner;
            public string tournament;
            public string round;
            public uint timeSubmitted;
            public uint timeUpdated;
            public uint reward;
            public string[] referencedIn;
            public uint positiveVotes;
            public uint negativeVotes;
        }

        public struct SubmissionDetails
        {
            public string title;
            public string descHash;
            public string fileHash;
            public List<BigInteger> distribution;
            public List<string> contributors;
            public List<string> references;
        }

        [Function("getVersion")]
        public class GetVersionFunction : FunctionMessage { }
        [Function("getTournament")]
        public class GetTournamentFunction : FunctionMessage { }
        [Function("getRound")]
        public class GetRoundFunction : FunctionMessage { }

        [Function("getOwner", "address")]
        public class GetOwnerFunction : FunctionMessage { }
        [Function("getTitle", "bytes32[3]")]
        public class GetTitleFunction : FunctionMessage { }
        [Function("getDescriptionHash", "bytes32[2]")]
        public class GetDescriptionHashFunction : FunctionMessage { }
        [Function("getFileHash", "bytes32[2]")]
        public class GetFileHashFunction : FunctionMessage { }
        [Function("getDistribution", "uint256[]")]
        public class GetDistributionFunction : FunctionMessage { }
        [Function("getContributors", "address[]")]
        public class GetContributorsFunction : FunctionMessage { }
        [Function("getReferences", "address[]")]
        public class GetReferencesFunction : FunctionMessage { }
        [Function("getTimeSubmitted", "uint256")]
        public class GetTimeSubmittedFunction : FunctionMessage { }
        [Function("getTimeUpdated", "uint256")]
        public class GetTimeUpdatedFunction : FunctionMessage { }
        [Function("getReward", "uint256")]
        public class GetRewardFunction : FunctionMessage { }
        [Function("getReferencedIn", "address[]")]
        public class GetReferencedInFunction : FunctionMessage { }
        [Function("getVotes", "(uint256,uint256)")]
        public class GetVotesFunction : FunctionMessage { }
        [Function("getViewers", "address[]")]
        public class GetViewersFunction : FunctionMessage { }
        [Function("getBalance", "uint256")]
        public class GetBalanceFunction : FunctionMessage { }
        [Function("getTotalWinnings", "uint256")]
        public class GetTotalWinningsFunction : FunctionMessage { }
        [Function("getData", typeof(SubmissionReturnDataDTO))]
        public class GetDataFunction : FunctionMessage { }

        [Function("unlockFile")]
        public class UnlockFileFunction : FunctionMessage { }
        [Function("updateDetails")]
        public class UpdateDetailsFunction : FunctionMessage
        {
            [Parameter("DetailsUpdates", 1)]
            public DetailsUpdates Updates { get; set; }
        }
        [Function("setContributorsAndReferences")]
        public class GetContributorsAndReferencesFunction : FunctionMessage { }
        [Function("flagMissingReference")]
        public class FlagMissingReferenceFunction : FunctionMessage
        {
            [Parameter("address", "missingReference", 1)]
            public string MissingReference { get; set; }
        }
        [Function("getAvailableReward", "uint256")]
        public class GetAvailableRewardFunction : FunctionMessage { }
        [Function("withdrawReward")]
        public class WithdrawRewardFunction : FunctionMessage { }

        [FunctionOutput]
        public class SubmissionReturnDataDTO : IFunctionOutputDTO
        {
            [Parameter("SubmissionInfo", 1)]
            public SubmissionInfoDTO Info { get; set; }
            [Parameter("SubmissionData", 1)]
            public SubmissionDetailsDTO Details { get; set; }
        }

        [FunctionOutput]
        public class SubmissionInfoDTO : IFunctionOutputDTO
        {
            [Parameter("address", "owner")]
            public string Owner { get; set; }
            [Parameter("address", "tournament")]
            public string Tournament { get; set; }
            [Parameter("address", "round")]
            public string Round { get; set; }
            [Parameter("uint256", "timeSubmitted")]
            public uint TimeSubmitted { get; set; }
            [Parameter("uint256", "timeUpdated")]
            public uint TimeUpdated { get; set; }
            [Parameter("uint256", "reward")]
            public uint Reward { get; set; }
            [Parameter("address[]", "referencedIn")]
            public string[] ReferencedIn { get; set; }
            [Parameter("uint256", "positiveVotes")]
            public uint PositiveVotes { get; set; }
            [Parameter("uint256", "negativeVotes")]
            public uint NegativeVotes { get; set; }
        }

        [FunctionOutput]
        public class SubmissionDetailsDTO : IFunctionOutputDTO
        {
            [Parameter("bytes32[3]", "title")]
            public string Title { get; set; }
            [Parameter("bytes32[2]", "descHash")]
            public string[] DescHash { get; set; }
            [Parameter("bytes32[2]", "fileHash")]
            public string[] FileHash { get; set; }
            [Parameter("uint256[]", "distribution")]
            public uint[] Distribution { get; set; }
            [Parameter("address[]", "contributors")]
            public string[] Contributors { get; set; }
            [Parameter("address[]", "references")]
            public string[] References { get; set; }
        }

        [FunctionOutput]
        public class DetailsUpdates : IFunctionOutputDTO
        {
            [Parameter("bytes32", "title", 3)]
            public string Title { get; set; }
            [Parameter("bytes32", "descHash", 2)]
            public string[] DescHash { get; set; }
            [Parameter("bytes32", "fileHash", 2)]
            public string[] FileHash { get; set; }
        }



        public IEnumerator getTitle()
        {
            if (details.title != null)
            {
                yield return details.title;
            }

            var titleRequest = new QueryUnityRequest<GetTitleFunction, EthereumTypes.Bytes32_3>(Network.infuraProvider, Network.account);
            yield return titleRequest.Query(address);
        }

        public IEnumerator getDescriptionHash()
        {
            if (description != null)
            {
                yield return description;
            }

            var descriptionHashRequest = new QueryUnityRequest<GetDescriptionHashFunction, EthereumTypes.Bytes32_2>(Network.infuraProvider, Network.account);
            yield return descriptionHashRequest.Query(address);
        }

        public IEnumerator SubmitToTournament()
        {
            yield return 2;

            Async main = Async.runInCoroutine(delegate (Async thread)
            {
                return MatryxPlatform.hasEnteredMatryx(thread, Network.account);
            });
            main.onEvent("hasEnteredMatryx", delegate (object datas)
            {
                if(!(bool)datas)
                {
                    main = Async.runInCoroutine(delegate (Async thread)
                    {
                        return MatryxPlatform.enterMatryx(thread);
                    });
                    main.onEvent("enterMatryx-success", delegate (object datas2)
                    {

                    });
                }
            });

            //// tournament.entryFee();
            //var submission = (Submission)((object[])context.param)[0];
            //tournamentContract = new Contract(null, tournamentAbi, submission.tournamentAddress);
            //var entryFeeFunction = tournamentContract.GetFunction("entryFee");
            //var entryFeeInput = entryFeeFunction.CreateCallInput(new object[0]);
            //entryFeeInput.Gas = new HexBigInteger(300000);
            //var entryFeeCall = new EthCallUnityRequest(mtxNode);
            //yield return entryFeeCall.SendRequest(entryFeeInput, BlockParameter.CreateLatest());
            //EntryFeeDTO entryFee = new EntryFeeDTO();
            //try
            //{
            //    entryFee = entryFeeFunction.DecodeDTOTypeOutput<EntryFeeDTO>(entryFeeCall.Result);
            //}
            //catch (Exception e)
            //{
            //    Debug.Log("Could not get tournament entry fee.");
            //}

            //// token.approve(tournament.address, tournament.entryFee)
            //if (entryFee.entryFee > 0)
            //{

            //    var tokenApproveFunction = tokenContract.GetFunction("approve");
            //    object[] tokenClearApproveParams = { submission.tournamentAddress, 0 };
            //    object[] tokenApproveParams = { submission.tournamentAddress, entryFee.entryFee };
            //    var tokenClearApproveInput = tokenApproveFunction.CreateTransactionInput(usedAccount, tokenClearApproveParams);
            //    var tokenApproveInput = tokenApproveFunction.CreateTransactionInput(usedAccount, tokenApproveParams);
            //    tokenClearApproveInput.Gas = new HexBigInteger(3000000);
            //    tokenApproveInput.Gas = new HexBigInteger(3000000);
            //    var tokenClearApproveTransaction = new EthSendTransactionUnityRequest(mtxNode);
            //    var tokenApproveTransaction = new EthSendTransactionUnityRequest(mtxNode);
            //    yield return tokenClearApproveTransaction.SendRequest(tokenClearApproveInput);
            //    try
            //    {
            //        var tokenClearApprovalResult = tokenClearApproveTransaction.Result;
            //    }
            //    catch (Exception e)
            //    {
            //        Debug.Log("Could not approve tournament to withdraw entry fee.");
            //    }

            //    yield return tokenApproveTransaction.SendRequest(tokenApproveInput);
            //    try
            //    {
            //        var tokenApprovalTransactionResult = tokenApproveTransaction.Result;
            //    }
            //    catch (Exception e)
            //    {
            //        Debug.Log("Could not approve tournament to withdraw entry fee.");
            //    }
            //}

            ////platform.enterTournament(tournament.address)
            //var enterTournamentFunction = platformContract.GetFunction("enterTournament");
            //object[] enterTournamentParams = { submission.tournamentAddress };
            //var enterTournamentInput = enterTournamentFunction.CreateTransactionInput(usedAccount, enterTournamentParams);
            //enterTournamentInput.Gas = new HexBigInteger(3300000);
            //var enterTournamentTransaction = new EthSendTransactionUnityRequest(mtxNode);
            //yield return enterTournamentTransaction.SendRequest(enterTournamentInput);
            //try
            //{
            //    var enterTournamentTransactionResult = enterTournamentTransaction.Result;
            //}
            //catch (Exception e)
            //{
            //    Debug.Log("Could not enter tournament.");
            //}

            //// Fix the below code
            //// Prepare create submission
            //var createSubmissionFunction = tournamentContract.GetFunction("createSubmission");
            //// Parse routine params
            //var tournamentAddress = (submission.tournamentAddress);
            //var title = submission.title;

            //WWWForm form = new WWWForm();
            //form.AddBinaryData("description", Encoding.ASCII.GetBytes(((Submission)((object[])(context.param))[0]).title), "description.txt", "text/plain");
            //form.AddBinaryData("jsonContent", Encoding.ASCII.GetBytes(((Submission)((object[])(context.param))[0]).body), "jsonContent.json", "application/json");
            //UnityWebRequest ipfsRequest = UnityWebRequest.Post(uploadURL, form);
            //yield return ipfsRequest.Send();
            //print("request completed with code: " + ipfsRequest.responseCode);
            //if (ipfsRequest.isNetworkError)
            //{
            //    print("Error: " + ipfsRequest.error);
            //}
            //else
            //{
            //    print("Request Response: " + ipfsRequest.downloadHandler.text);
            //}

            //var response = serializer.Deserialize<object>(ipfsRequest.downloadHandler.data) as Dictionary<string, object>;
            //var folderHash = Encoding.UTF8.GetBytes((string)response["folderHash"]);
            //string[] contributors = { };
            //int[] contributorDistribution = { };
            //string[] references = { };

            //if (submission.references != string.Empty)
            //{
            //    references = submission.references.Split(',');
            //}
            //if (submission.contributors != string.Empty)
            //{
            //    contributors = submission.contributors.Split(',');
            //    contributorDistribution = Enumerable.Range(0, contributors.Length).Select(x => 1).ToArray();
            //}

            //// Make input
            //object[] inputParams = { title, usedAccount, folderHash, contributors, contributorDistribution, references };
            //var transactionInput = createSubmissionFunction.CreateTransactionInput(usedAccount, inputParams);
            //transactionInput.Gas = new HexBigInteger(3000000);
            //transactionInput.GasPrice = new HexBigInteger(20);
            //// Do request
            //var requestTransaction = new EthSendTransactionUnityRequest(mtxNode);
            //yield return requestTransaction.SendRequest(transactionInput);
            //// Results
            //try
            //{
            //    var resultsTransaction = requestTransaction.Result;
            //    // Success
            //    context.done(resultsTransaction);
            //}
            //catch (Exception e)
            //{
            //    // Error
            //    Debug.Log("Could not submit submission");
            //    //Debug.Log(e);
            //    context.done(null);
            //}
        }
    }
}