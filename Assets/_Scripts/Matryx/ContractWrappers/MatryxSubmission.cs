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
using Calcflow.UserStatistics;
using static Matryx.MatryxTournament;

namespace Matryx
{
    public class MatryxSubmission
    {
        public MatryxSubmission(MatryxTournament tournament, string title, string description=null, string content=null, int value = 1)
        {
            this.tournament = tournament;
            this.data = new SubmissionDataDTO() { TournamentAddress = tournament.address };
            this.title = title;
            this.description = description;
            this.data.ContentHash = "";// "QmTDNWPTf6nM5sAwqKN1unTqvRDhr5sDxDEkLRMxbwAokz";
            commit = new MatryxCommit(content, value);
        }
        public MatryxSubmission(string hash)
        {
            this.hash = hash;
            this.data = new SubmissionDataDTO();
        }
        public MatryxSubmission(string title, string hash)
        {
            this.data = new SubmissionDataDTO();
            this.title = title;
            this.hash = hash;
        }

        public string title = "";
        public string description;
        public string hash;
        public MatryxTournament tournament;
        public SubmissionDataDTO data;
        public MatryxCommit commit;

        public bool calcflowCompatible = true;
        public string EquationJson
        {
            get
            {
                if (!calcflowCompatible) return ""; else return commit.content;
            }
        }

        [FunctionOutput]
        public class SubmissionDataDTO : IFunctionOutputDTO
        {
            [Parameter("address", "tournament", 1)]
            public string TournamentAddress { get; set; }
            [Parameter("uint256", "roundIndex", 2)]
            public long RoundIndex { get; set; }
            [Parameter("bytes32", "commitHash", 3)]
            public BigInteger CommitHash { get; set; }
            [Parameter("string", "content", 4)]
            public string ContentHash { get; set; }
            [Parameter("uint256", "reward", 5)]
            public BigInteger Reward { get; set; }
            [Parameter("uint256", "timestamp", 6)]
            public BigInteger Timestamp { get; set; }
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

        public IEnumerator get()
        {
            var url = MatryxCortex.submissionURL+hash;
            using (var submissionWWW = new WWW(url))
            {
                yield return submissionWWW;
                var res = MatryxCortex.serializer.Deserialize<object>(submissionWWW.bytes) as Dictionary<string, object>;
                var data = res["data"] as Dictionary<string, object>;
                var submission = data["submission"] as Dictionary<string, object>;

                this.title = submission["title"] as string;
                this.description = submission["description"] as string;
                this.hash = submission["hash"] as string;
                this.tournament.address = submission["tournament"] as string;
                this.commit.hash = (submission["commit"] as Dictionary<string, object>)["hash"] as string;
                this.commit.ipfsContentHash = (submission["commit"] as Dictionary<string, object>)["ipfsContent"] as string;
                this.data.TournamentAddress = submission["tournament"] as string;
                int? roundIndex = submission["roundIndex"] as int?;
                this.data.RoundIndex = roundIndex.Value;
                this.data.CommitHash = BigInteger.Parse(this.commit.hash, System.Globalization.NumberStyles.HexNumber);
                this.data.ContentHash = submission["ipfsContent"] as string;
                long? reward = submission["reward"] as long?;
                this.data.Reward = new BigInteger(reward.Value);
                long? timestamp = submission["timestamp"] as long?;
                this.data.Timestamp = new BigInteger(timestamp.Value);

                // TODO: Maybe optimize this by performing a separate call upon opening each submission
                var ipfsURL = "https://ipfs.infura.io:5001/api/v0/object/get?arg=";
                using (WWW ipfsWWW = new WWW(ipfsURL + commit.ipfsContentHash+"?encoding=json"))
                {
                    yield return ipfsWWW;
                    var ipfsObj = MatryxCortex.serializer.Deserialize<object>(ipfsWWW.bytes) as Dictionary<string, object>;
                    var links = ipfsObj["Links"] as List<Dictionary<string, object>>;
                    // TODO: Make better when you introduce preview images
                    commit.ipfsContentHash = links[0]["Hash"] as string;
                }

                using (WWW ipfsWWW2 = new WWW(ipfsURL + commit.ipfsContentHash + "?encoding=json"))
                {
                    yield return ipfsWWW2;
                    var ipfsObj2 = MatryxCortex.serializer.Deserialize<object>(ipfsWWW2.bytes) as Dictionary<string, object>;
                    var ipfsData = ipfsObj2["Data"] as string;
                    commit.content = ipfsData;
                }

                var ESSRegEx = "{.*rangeKeys.*rangePairs.*ExpressionKeys.*ExpressionValues.*}";
                calcflowCompatible = Regex.IsMatch(commit.content, ESSRegEx);
            }
        }

        public IEnumerator uploadContent()
        {
            if (data.ContentHash == null || data.ContentHash.Equals(string.Empty))
            {
                if (description != null && !description.Equals(string.Empty))
                {
                    if(commit.ipfsContentHash != null && commit.ipfsContentHash.Substring(0, 2) == "Qm")
                    {
                        var uploadToIPFS = new Utils.CoroutineWithData<string>(MatryxCortex.Instance, Utils.uploadJson(title, description, commit.ipfsContentHash));
                        yield return uploadToIPFS;
                        data.ContentHash = uploadToIPFS.result;
                    }
                }
            }
        }

        public IEnumerator submit(Async.EventDelegate callback=null)
        {
            StatisticsTracking.StartEvent("Matryx", "Submission Creation");

            ResultsMenu.transactionObject = this;

            var isEntrant = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, tournament.isEntrant(NetworkSettings.activeAccount));
            yield return isEntrant;

            var tournamentInfo = new Utils.CoroutineWithData<TournamentInfo>(MatryxCortex.Instance, tournament.getInfo());
            yield return tournamentInfo;

            if (tournament.owner.Equals(NetworkSettings.activeAccount, System.StringComparison.CurrentCultureIgnoreCase))
            {
                ResultsMenu.Instance.PostFailure(this, "You own this tournament; Unable to create submission.");
                yield break;
            }

            if(!isEntrant.result)
            {
                var allowance = new Utils.CoroutineWithData<BigInteger>(MatryxCortex.Instance, MatryxToken.allowance(NetworkSettings.activeAccount, MatryxPlatform.address));
                yield return allowance;

                Debug.Log(tournament.entryFee);
                if (allowance.result < tournament.entryFee)
                {
                    ResultsMenu.Instance.SetStatus("Approving entry fee...");

                    if (allowance.result != BigInteger.Zero)
                    {
                        var approveZero = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, MatryxToken.approve(MatryxPlatform.address, BigInteger.Zero));
                        yield return approveZero;

                        if (!approveZero.result)
                        {
                            Debug.Log("Failed to reset tournament's allowance to zero for this user. Please check the allowance this user has granted the tournament");
                            ResultsMenu.Instance.PostFailure(this);
                            yield break;
                        }
                    }

                    var approveEntryFee = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, MatryxToken.approve(MatryxPlatform.address, tournament.entryFee));
                    yield return approveEntryFee;

                    if (!approveEntryFee.result)
                    {
                        Debug.Log("Failed to set the tournament's allowance from this user to the tournament's entry fee");
                        ResultsMenu.Instance.PostFailure(this);
                        yield break;
                    }
                }

                ResultsMenu.Instance.SetStatus("Entering Tournament...");

                var enterTournament = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, tournament.enter());
                yield return enterTournament;

                if (!enterTournament.result)
                {
                    Debug.Log("Failed to enter tournament");
                    ResultsMenu.Instance.PostFailure(this);
                    yield break;
                }
            }

            ResultsMenu.Instance.SetStatus("Claiming Commit...");
            yield return commit.claim();

            ResultsMenu.Instance.SetStatus("Creating Commit...");
            yield return commit.create(null);

            ResultsMenu.Instance.SetStatus("Uploading Submission Content to IPFS...");
            yield return uploadContent();

            if(!data.ContentHash.Contains("Qm"))
            {
                Debug.Log("Failed to upload file to IPFS");
                yield break;
            }

            ResultsMenu.Instance.SetStatus("Creating Submission...");
            var createSubmission = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, tournament.createSubmission(this));
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