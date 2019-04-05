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

using Matryx;
using System.Numerics;
using Nanome.Core;
using System.Collections.Generic;
using System.Collections;
using System;
using System.Text;

namespace Matryx
{
    public class MatryxCommit
    {
        public static string address = "";
        public static string ABI = "";
        public static Contract contract;

        public string owner = "";
        public BigInteger timestamp;
        public string groupHash = "";
        public string hash = "";
        public string ipfsContentHash = "";
        public BigInteger value;
        public BigInteger ownerTotalValue;
        public BigInteger totalValue;
        public BigInteger height;
        public string parentHash = "";
        public List<string> children;

        public bool fork;

        public string content = "";
        public string Content
        {
            get
            {
                return content;
            }
            set
            {
                content = value;
                ipfsContentHash = "";
            }
        }

        public static Dictionary<string, Claim> contentClaims = new Dictionary<string, Claim>();
        public class Claim
        {
            public string sender;
            public string content;
            public byte[] salt;
            public byte[] claimHash;

            public Claim(string contentHash, string cont)
            {
                if (cont.Equals(String.Empty))
                {
                    throw new System.Exception("Commit must have content");
                }

                if (!contentHash.Contains("Qm"))
                {
                    throw new System.Exception("Commit content must already have been uploaded");
                }

                sender = NetworkSettings.activeAccount;
                content = cont;
                var saltString = Utils.GetRandomHexNumber(64);
                salt = Utils.HexStringToByteArray(saltString);
                var hexContentHash = "0x" + BitConverter.ToString(Encoding.Default.GetBytes(contentHash)).Replace("-", "");
                claimHash = Utils.HexStringToByteArray("0x" + new Nethereum.Util.Sha3Keccack().CalculateHashFromHex(sender, saltString, hexContentHash));
            }
        }

        public MatryxCommit(string cont, int val)
        {
            content = cont;
            value = Nethereum.Util.UnitConversion.Convert.ToWei(val);
        }

        public static void setContract(string abi, string addr)
        {
            address = addr;
            ABI = abi;
            contract = new Nethereum.Contracts.Contract(null, abi, addr);
        }
        
        //function getCommit(bytes32 commitHash) external view returns(LibCommit.Commit memory commit);
        [Function("getCommit", typeof(Commit))]
        public class GetCommitFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public string CommitHash { get; set; }
        }

        [FunctionOutput]
        public class Commit : IFunctionOutputDTO
        {
            [Parameter("address")]
            public string Owner { get; set; }
            [Parameter("uint256")]
            public BigInteger Timestamp { get; set; }
            [Parameter("bytes32")]
            public string GroupHash { get; set; }
            [Parameter("bytes32")]
            public string CommitHash { get; set; }
            [Parameter("string")]
            public string ContentHash { get; set; }
            [Parameter("uint256")]
            public string Value { get; set; }
            [Parameter("uint256")]
            public string OwnerTotalValue { get; set; }
            [Parameter("uint256")]
            public string TotalValue { get; set; }
            [Parameter("uint256")]
            public string Height { get; set; }
            [Parameter("bytes32")]
            public string ParentHash { get; set; }
            [Parameter("bytes32[]")]
            public string Children { get; set; }
        }

        //function getBalance(bytes32 commitHash) external view returns(uint256);
        [Function("getBalance", "uint256")]
        public class GetBalanceFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public string CommitHash { get; set; }
        }

        //function getCommitByContent(string calldata content) external view returns(LibCommit.Commit memory commit);
        [Function("getCommitByContent", typeof(Commit))]
        public class GetCommitByContentFunction : FunctionMessage
        {
            [Parameter("string")]
            public string Content { get; set; }
        }

        //function getInitialCommits() external view returns(bytes32[] memory);
        [Function("getInitialCommits", "bytes32[]")]
        public class GetInitialCommitsFunction: FunctionMessage {}

        //function getGroupMembers(bytes32 commitHash) external view returns(address[] memory);
        [Function("getGroupMembers", "bytes32")]
        public class GetGroupMembersFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public string CommitHash { get; set; }
        }

        //function getSubmissionsForCommit(bytes32 commitHash) external view returns(bytes32[] memory);
        [Function("getSubmissionForCommit", "bytes32[]")]
        public class GetSubmissionsForCommitFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public string CommitHash { get; set; }
        }

        //function addGroupMember(bytes32 commitHash, address member) external;
        [Function("addGroupMember")]
        public class AddGroupMemberFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public string CommitHash { get; set; }
            [Parameter("address")]
            public string Member { get; set; }
        }

        //function addGroupMembers(bytes32 commitHash, address[] calldata members) external;
        [Function("addGroupMembers")]
        public class AddGroupMembersFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public string CommitHash { get; set; }
            [Parameter("address[]")]
            public List<string> Members { get; set; }
        }

        //function claimCommit(bytes32 commitHash) external;
        [Function("claimCommit")]
        public class ClaimCommitFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public byte[] ClaimHash { get; set; }
        }

        //function createCommit(bytes32 parentHash, bool isFork, bytes32 salt, string calldata content, uint256 value) external;
        [Function("createCommit")]
        public class CreateCommitFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public string ParentHash { get; set; }
            [Parameter("bool")]
            public bool IsFork { get; set; }
            [Parameter("bytes32")]
            public byte[] Salt { get; set; }
            [Parameter("string")]
            public string ContentHash { get; set; }
            [Parameter("uint256")]
            public BigInteger Value { get; set; }
        }

        //function createSubmission(address tAddress, string calldata content, bytes32 parentHash, bool isFork, bytes32 salt, string calldata commitContent, uint256 value) external;
        [Function("createSubmission")]
        public class CreateSubmissionFunction : FunctionMessage
        {
            [Parameter("address")]
            public string TournamentAddress { get; set; }
            [Parameter("string")]
            public string SubmissionContentHash { get; set; }
            [Parameter("bytes32")]
            public string ParentHash { get; set; }
            [Parameter("bool")]
            public bool IsFork { get; set; }
            [Parameter("bytes32")]
            public byte[] Salt { get; set; }
            [Parameter("string")]
            public string CommitContentHash { get; set; }
            [Parameter("uint256")]
            public BigInteger Value { get; set; }
        }

        //function getAvailableRewardForUser(bytes32 commitHash, address user) external view returns(uint256);
        [Function("getAvailableRewardForUser", "uint256")]
        public class GetAvailableRewardForUserFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public string CommitHash { get; set; }
            [Parameter("address")]
            public string User { get; set; }
        }

        //function withdrawAvailableReward(bytes32 commitHash) external;
        [Function("withdrawAvailableReward")]
        public class WithdrawAvailableRewardFunction : FunctionMessage
        {
            [Parameter("bytes32")]
            public string CommitHash { get; set; }
        }

        public static IEnumerator getCommit(string commitHash, Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<GetCommitFunction, Commit>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new GetCommitFunction() { CommitHash = commitHash }, MatryxCommit.address);
            yield return queryRequest.Result;
        }

        public static IEnumerator getBalance(string commitHash, Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<GetBalanceFunction, EthereumTypes.Uint256>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new GetBalanceFunction() { CommitHash = commitHash }, MatryxCommit.address);
            yield return queryRequest.Result;
        }

        public static IEnumerator getCommitByContent(string content, Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<GetCommitByContentFunction, Commit>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new GetCommitByContentFunction() { Content = content }, MatryxCommit.address);
            yield return queryRequest.Result;
        }

        public static IEnumerator getInitialCommits(Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<GetInitialCommitsFunction, EthereumTypes.Bytes32Array>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new GetInitialCommitsFunction() {}, MatryxCommit.address);
            yield return queryRequest.Result;
        }

        public static IEnumerator getSubmissionsForCommit(string commitHash, Async thread = null)
        {
            var queryRequest = new QueryUnityRequest<GetSubmissionsForCommitFunction, EthereumTypes.Bytes32Array>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new GetSubmissionsForCommitFunction() { CommitHash = commitHash }, MatryxCommit.address);
            yield return queryRequest.Result;
        }

        public static IEnumerator addGroupMember(string commitHash, string member, Async thread = null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.activePrivateKey, NetworkSettings.activeAccount);
            var addGroupMemberMsg = new AddGroupMemberFunction() { CommitHash = commitHash, Member = member, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice };
            yield return transactionRequest.SignAndSendTransaction<AddGroupMemberFunction>(addGroupMemberMsg, MatryxCommit.address);

            var txStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "addGroupMember", thread));
            yield return txStatus;
            yield return txStatus.result;
        }

        public static IEnumerator addGroupMembers(string commitHash, List<string> members, Async thread = null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.activePrivateKey, NetworkSettings.activeAccount);
            var addGroupMembersMsg = new AddGroupMembersFunction() { CommitHash = commitHash, Members = members, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice };
            yield return transactionRequest.SignAndSendTransaction<AddGroupMembersFunction>(addGroupMembersMsg, MatryxCommit.address);

            var txStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "addGroupMembers", thread));
            yield return txStatus;
            yield return txStatus.result;
        }

        public IEnumerator uploadContent()
        {
            ResultsMenu.Instance.SetStatus("Uploading Your Content...");
            if (!content.Equals(""))
            {
                List<string> fileNames = new List<string>() { "jsonContent.json" };
                List<byte[]> contents = new List<byte[]>() { MatryxCortex.serializer.Serialize(content) };
                List<string> fileTypes = new List<string>() { "application/json" };
                var uploadToIPFS = new Utils.CoroutineWithData<string>(MatryxCortex.Instance, Utils.uploadFiles(fileNames, contents, fileTypes));
                yield return uploadToIPFS;
                ipfsContentHash = uploadToIPFS.result;
                yield return true;
            }

            yield return false;
        }

        public static Claim getClaim(string ipfsHash)
        {
            return contentClaims.ContainsKey(ipfsHash) ? contentClaims[ipfsHash] : null;
        }

        public IEnumerator claim(Async thread = null)
        {
            // Calculate commithash from content
            var uploader = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, uploadContent());
            yield return uploader;

            Claim claim = new Claim(ipfsContentHash, content);
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.activePrivateKey, NetworkSettings.activeAccount);
            var claimCommitMsg = new ClaimCommitFunction() { ClaimHash = claim.claimHash, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice };
            yield return transactionRequest.SignAndSendTransaction<ClaimCommitFunction>(claimCommitMsg, MatryxCommit.address);

            var txStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "claimCommit", thread));
            yield return txStatus;
            if (txStatus.result)
            {
                contentClaims.Add(ipfsContentHash, claim);
            }

            yield return txStatus.result;
        }

        public IEnumerator create(Async thread = null)
        {
            Claim claim = getClaim(ipfsContentHash);
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.activePrivateKey, NetworkSettings.activeAccount);
            var createCommitMsg = new CreateCommitFunction()
            {
                ParentHash = parentHash,
                IsFork = fork,
                Salt = claim.salt,
                ContentHash = ipfsContentHash,
                Value = value,
                Gas = NetworkSettings.txGas,
                GasPrice = NetworkSettings.txGasPrice
            };
            yield return transactionRequest.SignAndSendTransaction<CreateCommitFunction>(createCommitMsg, MatryxCommit.address);

            var txStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "createCommit", thread));
            yield return txStatus;
            // Get commit hash and assign to this commit
            var getCommit = new Utils.CoroutineWithData<Commit>(MatryxCortex.Instance, getCommitByContent(ipfsContentHash));
            yield return getCommit;
            hash = getCommit.result.CommitHash;
            yield return txStatus.result;
        }

        public IEnumerator createSubmission(string tournamentAddress, string parentHash, bool isFork, BigInteger value, Async thread = null)
        {
            var salt = getClaim(ipfsContentHash).salt;
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.activePrivateKey, NetworkSettings.activeAccount);
            var createSubmissionMsg = new CreateSubmissionFunction()
            {
                TournamentAddress = tournamentAddress,
                SubmissionContentHash = ipfsContentHash,
                ParentHash = parentHash,
                IsFork = isFork,
                Salt = salt,
                CommitContentHash = ipfsContentHash,
                Value = value,
                Gas = NetworkSettings.txGas,
                GasPrice = NetworkSettings.txGasPrice
            };
            yield return transactionRequest.SignAndSendTransaction<CreateSubmissionFunction>(createSubmissionMsg, MatryxCommit.address);

            var txStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "createSubmission", thread));
            yield return txStatus;
            yield return txStatus.result;
        }

        public static IEnumerator getAvailableRewardForUser(string commitHash, string user)
        {
            var queryRequest = new QueryUnityRequest<GetAvailableRewardForUserFunction, Commit>(NetworkSettings.infuraProvider, NetworkSettings.activeAccount);
            yield return queryRequest.Query(new GetAvailableRewardForUserFunction() { CommitHash = commitHash, User = user }, MatryxCommit.address);
        }

        public static IEnumerator withdrawAvailableReward(string commitHash, Async thread = null)
        {
            var transactionRequest = new TransactionSignedUnityRequest(NetworkSettings.infuraProvider, NetworkSettings.activePrivateKey, NetworkSettings.activeAccount);
            var withdrawRewardMsg = new WithdrawAvailableRewardFunction() { CommitHash = commitHash, Gas = NetworkSettings.txGas, GasPrice = NetworkSettings.txGasPrice };
            yield return transactionRequest.SignAndSendTransaction<WithdrawAvailableRewardFunction>(withdrawRewardMsg, MatryxCommit.address);

            var txStatus = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, Utils.GetTransactionStatus(transactionRequest, "withdrawAvailableReward", thread));
            yield return txStatus;
            yield return txStatus.result;
        }
    }
}