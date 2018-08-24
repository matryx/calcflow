using UnityEngine;

using System;
using System.Collections.Generic;

namespace MatryxJsonRpc
{

    public class Submission
    {
        // TODO
        // Submission no longer has body but now has "description" and "fileHash"
        // "fileHash" is in the form "Qm...", and file content can be found at
        // IPFS_URL = https://ipfs.io/ipfs/<fileHash>
        // if json content, is found at IPFS_URL/jsonContent.json
        // example: https://ipfs.io/ipfs/QmUtR6gEeBRzsGpVquqQZQK8sVUC3h4jHxvK8UbkutLjHL

        public string title;
        public string body;

        public string author;

        public string address;
        public string tournamentAddress;

        public string references;
        public string contributors;

        public List<string> contributorsList()
        {
            return new List<string>(this.contributors.Split('\n'));
        }
        public List<string> referencesList()
        {
            return new List<string>(this.references.Split('\n'));
        }

        public void contributorsList(List<string> contributors)
        {
            this.contributors = string.Join("\n", contributors.ToArray());
        }
        public void referencesList(List<string> references)
        {
            this.references = string.Join("\n", references.ToArray());
        }

    }

}
