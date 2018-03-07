using UnityEngine;

using System;

namespace MatryxJsonRpc
{

    public class Tournament
    {
        public long id;
        public long bounty;

        public string title;
        public string description;

        public string address;

        public string UniqueId
        {
            get
            {
                // return address;
                return id.ToString();
            }
        }

    }

}

