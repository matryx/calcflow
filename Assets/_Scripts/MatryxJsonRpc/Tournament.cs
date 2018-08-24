using UnityEngine;

using System;

namespace MatryxJsonRpc
{

    public class Tournament
    {
        // TODO
        // Tournament now "fileHash"
        // "fileHash" is in the form "Qm...", and file content can be found at
        // IPFS_URL = https://ipfs.io/ipfs/<fileHash>
        // if json content, is found at IPFS_URL/jsonContent.json
        // example: https://ipfs.io/ipfs/QmUtR6gEeBRzsGpVquqQZQK8sVUC3h4jHxvK8UbkutLjHL

        public long bounty;

        public string title;
        public string description;

        public string address;
    }

}
