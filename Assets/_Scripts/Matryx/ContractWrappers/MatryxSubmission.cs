using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Nethereum.ABI.FunctionEncoding.Attributes;
using Nethereum.ABI.Model;
using Nethereum.Contracts;
using Nethereum.JsonRpc.UnityClient;
using Nanome.Core;
using System.Numerics;
using UnityEngine.Networking;
using System.Text;

using Nanome.Maths.Serializers.JsonSerializer;
using System.Linq;
using System.Text.RegularExpressions;

namespace Matryx
{
    public class MatryxSubmission
    {
        public MatryxSubmission(MatryxTournament tournament, string title, List<string> contributors, List<string> references, string file=null, string description=null)
        {
            this.tournament = tournament;
            this.details = new SubmissionDetails();
            this.details.title = title;
            this.details.contributors = contributors;
            this.details.distribution = Enumerable.Range(0, details.contributors.Count + 1).Select(x => new BigInteger(1)).ToList();
            this.details.references = references;
            this.details.descHash = "";// "QmTDNWPTf6nM5sAwqKN1unTqvRDhr5sDxDEkLRMxbwAokz";
            this.file = file;
            this.description = description;
        }
        public MatryxSubmission(string address)
        {
            this.address = address;
            this.details = new SubmissionDetails();
        }
        public MatryxSubmission(string title, string address)
        {
            this.details = new SubmissionDetails();
            this.details.title = title;
            this.address = address;
        }

        public string address;
        public MatryxTournament tournament;
        public SubmissionInfo info;
        public SubmissionDetails details;

        public string description;
        public string file;
        public bool calcflowCompatible = true;
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
            public string timeSubmitted;
            public uint timeUpdated;
            public int reward;
            public string[] referencedIn;
            public uint positiveVotes;
            public uint negativeVotes;
        }

        public class SubmissionDetails
        {
            public string title;
            public List<string> contributors;
            public List<BigInteger> distribution;
            public List<string> references;
            public string descHash;
            public string fileHash;
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
            [Parameter("bytes32[3]", "title")]
            public string Title { get; set; }
            [Parameter("bytes32[2]", "descHash")]
            public string[] DescHash { get; set; }
            [Parameter("bytes32[2]", "fileHash")]
            public string[] FileHash { get; set; }
        }

        public IEnumerator getTournament()
        {
            var tournamentRequest = new QueryUnityRequest<GetTournamentFunction, EthereumTypes.Address>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return tournamentRequest.Query(address);
            yield return tournamentRequest.Result.Value;
        }

        public IEnumerator getTitle()
        {
            if (details.title != null)
            {
                yield return details.title;
            }

            var titleRequest = new QueryUnityRequest<GetTitleFunction, EthereumTypes.Bytes32_3>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return titleRequest.Query(address);
            yield return titleRequest.Result.Value;
        }

        public IEnumerator getDescriptionHash()
        {
            if (description != null)
            {
                yield return description;
            }

            var descriptionHashRequest = new QueryUnityRequest<GetDescriptionHashFunction, EthereumTypes.Bytes32_2>(NetworkSettings.infuraProvider, NetworkSettings.address);
            yield return descriptionHashRequest.Query(address);
        }

        public IEnumerator getFile()
        {
            if(details.fileHash == null)
            {
                var descriptionHashRequest = new QueryUnityRequest<GetFileHashFunction, EthereumTypes.Bytes32_2>(NetworkSettings.infuraProvider, NetworkSettings.address);
                yield return descriptionHashRequest.Query(address);

                details.fileHash = descriptionHashRequest.Result.Value + descriptionHashRequest.Result.ValueTwo;
            }

            var url = "https://ipfs.infura.io:5001/api/v0/cat?arg=" + details.fileHash + "/jsonContent.json";
            using (var www = new WWW(url))
            {
                yield return www;
                file = www.text;
                // TODO: Get Alex's help here
                var ESSRegEx = "{.*rangeKeys.*rangePairs.*ExpressionKeys.*ExpressionValues.*}";
                calcflowCompatible = Regex.IsMatch(file, ESSRegEx);
            }
        }

        public IEnumerator submit(Async.EventDelegate callback=null)
        {
            ResultsMenu.transactionObject = this;

            var isEntrant = new Utils.CoroutineWithData<bool>(MatryxExplorer.Instance, tournament.isEntrant(NetworkSettings.address));
            yield return isEntrant;

            var tournamentOwner = new Utils.CoroutineWithData<string>(MatryxExplorer.Instance, tournament.getOwner());
            yield return tournamentOwner;

            if(tournamentOwner.result.Equals(NetworkSettings.address.ToLower()))
            {
                ResultsMenu.Instance.PostFailure(this, "You own this tournament; Unable to create submission.");
                yield break;
            }

            if(!isEntrant.result)
            {
                var allowance = new Utils.CoroutineWithData<BigInteger>(MatryxExplorer.Instance, MatryxToken.allowance(NetworkSettings.address, MatryxPlatform.address));
                yield return allowance;

                Debug.Log(tournament.entryFee);
                if (allowance.result < tournament.entryFee)
                {
                    ResultsMenu.Instance.SetStatus("Approving entry fee...");

                    if (allowance.result != BigInteger.Zero)
                    {
                        var approveZero = new Utils.CoroutineWithData<bool>(MatryxExplorer.Instance, MatryxToken.approve(MatryxPlatform.address, BigInteger.Zero));
                        yield return approveZero;

                        if (!approveZero.result)
                        {
                            Debug.Log("Failed to reset tournament's allowance to zero for this user. Please check the allowance this user has granted the tournament");
                            ResultsMenu.Instance.PostFailure(this);
                            yield break;
                        }
                    }

                    var approveEntryFee = new Utils.CoroutineWithData<bool>(MatryxExplorer.Instance, MatryxToken.approve(MatryxPlatform.address, tournament.entryFee));
                    yield return approveEntryFee;

                    if (!approveEntryFee.result)
                    {
                        Debug.Log("Failed to set the tournament's allowance from this user to the tournament's entry fee");
                        ResultsMenu.Instance.PostFailure(this);
                        yield break;
                    }
                }

                ResultsMenu.Instance.SetStatus("Entering Tournament...");

                var enterTournament = new Utils.CoroutineWithData<bool>(MatryxExplorer.Instance, tournament.enter());
                yield return enterTournament;

                if (!enterTournament.result)
                {
                    Debug.Log("Failed to enter tournament");
                    ResultsMenu.Instance.PostFailure(this);
                    yield break;
                }
            }

            ResultsMenu.Instance.SetStatus("Uploading content to IPFS...");
            var uploadToIPFS = new Utils.CoroutineWithData<string[]>(MatryxExplorer.Instance, Utils.uploadToIPFS(this));
            yield return uploadToIPFS;

            if(uploadToIPFS.result == null || !uploadToIPFS.result[0].Contains("Qm"))
            {
                Debug.Log("Failed to upload file to IPFS");
                yield break;
            }

            if (uploadToIPFS.result[0] != null && !uploadToIPFS.result[0].Equals(string.Empty))
            {
                this.details.descHash = uploadToIPFS.result[0];
            }
            if (uploadToIPFS.result[1] != null && !uploadToIPFS.result[1].Equals(string.Empty))
            {
                this.details.fileHash = uploadToIPFS.result[1];
            }

            ResultsMenu.Instance.SetStatus("Creating Submission...");

            var createSubmission = new Utils.CoroutineWithData<bool>(MatryxExplorer.Instance, tournament.createSubmission(this));
            yield return createSubmission;

            if(callback != null)
            {
                callback(createSubmission.result);
            }

            if (!createSubmission.result)
            {
                Debug.Log("Failed to create submission");
                yield break;
            }
        }
    }
}